module Main (main) where

import Util
import Util.GPIO qualified as GPIO
import Util.Lifx

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.State.Strict
import Data.Bool
import Data.ByteString qualified as B
import Data.Char (isSpace)
import Data.Foldable
import Data.List
import Data.List.Extra
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Stream.Infinite qualified as Stream
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Data.Time
import Data.Tuple.Extra
import Data.Word
import Evdev (EventData (KeyEvent), KeyEvent (..))
import Evdev qualified
import Evdev.Codes (Key (..))
import Evdev.Stream
import GHC.Records (HasField)
import Lifx.Lan (HSBK, MonadLifx)
import Lifx.Lan qualified as Lifx
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString hiding (send)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi hiding (Event, error, get, head)
import Okapi qualified hiding (head)
import Optics
import Optics.State.Operators
import Options.Generic
import Spotify qualified
import Spotify.Servant.Player qualified as Spotify
import Spotify.Types.Artists qualified as Spotify
import Spotify.Types.Misc qualified as Spotify
import Spotify.Types.Search qualified as Spotify
import Spotify.Types.Simple qualified as Spotify
import Spotify.Types.Tracks qualified as Spotify
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import System.IO
import System.Process.Extra
import Text.Pretty.Simple

data Opts = Opts
    { gpioChip :: B.ByteString
    , buttonDebounce :: Double
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledIdleModePin :: Int
    , ledSendingModePin :: Int
    , ledNormalModePin :: Int
    , ledTvModePin :: Int
    , flashTime :: NominalDiffTime
    , lifxTimeout :: Double
    , lifxPort :: Word16
    , udpPort :: Word16
    , httpPort :: Warp.Port
    , spotifyDeviceId :: Spotify.DeviceID
    , keyboard :: [Text]
    , keySendPort :: PortNumber
    , keySendIps :: [IP]
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

data AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    , bulbs :: Stream.Stream (Lifx.Device, Lifx.LightState)
    , httpConnectionManager :: Manager
    , keySendSocket :: Socket
    , lightColourCache :: Maybe HSBK
    }
    deriving (Generic)
data Mode
    = Idle -- let the OS handle keypresses
    | Quiet -- just turn off LEDs
    | Sending -- send keypresses over UDP
    | Normal
    | TV
    deriving (Eq, Ord, Show, Enum, Bounded)
data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error
catchIO :: (MonadCatch m, MonadError Error m) => m a -> m a
catchIO = handleIOError $ throwError . Error "IO error when running action"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Pi"
    eventMVar <- newEmptyMVar

    let
        setLED :: (MonadState AppState m, MonadIO m, MonadLog Text m) => Int -> Bool -> m ()
        setLED pin =
            bool
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Just h -> GPIO.reset h >> modifying #activeLEDs (Map.delete pin)
                    Nothing -> logMessage $ "LED is already off: " <> showT pin
                )
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Nothing -> GPIO.set opts.gpioChip [pin] >>= modifying #activeLEDs . Map.insert pin
                    Just _ -> logMessage $ "LED is already on: " <> showT pin
                )

        handleError :: (MonadIO m, MonadState AppState m, MonadLog Text m) => Error -> m ()
        handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            setLED opts.ledErrorPin True

    -- TODO initialisation stuff - encapsulate this better somehow, without it being killed by LIFX failure
    -- TODO even discounting LIFX issue, unclear how to do this in Streamly 0.9, as there's no `Monad (Stream IO)`
    eventSocket <- socket AF_INET Datagram defaultProtocol
    bind eventSocket $ SockAddrInet (fromIntegral opts.udpPort) 0
    -- TODO disabled until logging is better - it's easier to see events when monitoring through a separate script
    -- let gpioMonitor =
    --         GPIO.mon opts.gpioChip (putMVar eventMVar . LogEvent) opts.buttonDebounce opts.buttonPin $
    --             -- TODO we'd ideally use this is a power button, but for noew we just monitor it
    --             -- since there have been issues with electrical interference causing spurious triggers
    --             putMVar eventMVar . LogEvent . ("GPIO button pressed: " <>) . showT =<< getCurrentTime
    let modeLED = \case
            Idle -> Just opts.ledIdleModePin
            Quiet -> Nothing
            Sending -> Just opts.ledSendingModePin
            Normal -> Just opts.ledNormalModePin
            TV -> Just opts.ledTvModePin
    httpConnectionManager <- newManager defaultManagerSettings
    keySendSocket <- socket AF_INET Datagram defaultProtocol >>= \s -> bind s (SockAddrInet defaultPort 0) >> pure s
    -- TODO shift this in to the LIFX block below - currently akward because this is needed to initialise state monad
    -- otherwise, what port to use? is it a bug that library doesn't release this soon enough? `* 2` is silly
    -- TODO log lights found
    -- TODO use existing logging and failure mechanisms when no lights found
    ds <-
        maybe (T.putStrLn "No LIFX devices found" >> exitFailure) pure
            . nonEmpty
            =<< either (\e -> T.putStrLn ("LIFX startup error: " <> showT e) >> exitFailure) pure
            =<< Lifx.runLifxT (lifxTime opts.lifxTimeout) (Just $ fromIntegral opts.lifxPort * 2) discoverLifx
    let initialState =
            AppState
                { activeLEDs = mempty
                , bulbs = Stream.cycle ds
                , lightColourCache = Nothing
                , ..
                }
        initialMode = Idle

    race_
        ( Warp.runSettings
            ( Warp.setLogger
                (curry3 $ unless . statusIsSuccessful . snd3 <*> putMVar eventMVar . ErrorEvent . Error "HTTP error")
                . Warp.setPort opts.httpPort
                $ Warp.defaultSettings
            )
            (webServer $ liftIO . putMVar eventMVar)
            -- TODO disabled - see `gpioMonitor` definition
            -- `race_` gpioMonitor
        )
        . flip runLoggingT (liftIO . T.putStrLn)
        . flip evalStateT initialState
        . runLifxUntilSuccess
            (either (handleError . Error @() "Misc exception") (handleError . Error "LIFX error"))
            (lifxTime opts.lifxTimeout)
            (Just $ fromIntegral opts.lifxPort)
        $ do
            S.fold
                ( SF.drainMapM \case
                    ErrorEvent e -> handleError e
                    LogEvent t -> logMessage t
                    ActionEvent f action -> (either handleError pure <=< runExceptT) $ runM do
                        r <-
                            action & translate \a -> do
                                logMessage $ showT a
                                runAction (opts & \Opts{..} -> ActionOpts{..}) a
                        sendM . logMessage $ showT r
                        sendM . liftIO $ f r
                )
                . S.concatMap S.fromList
                . S.append
                    -- flash all lights to show we have finished initialising
                    ( S.fromList
                        . map (pure . ActionEvent mempty . send)
                        $ concatMap
                            (\n -> [SetLED n True, Sleep 0.2, SetLED n False])
                            (mapMaybe modeLED enumerate <> [opts.ledErrorPin])
                            <> [Sleep 0.5]
                            <> maybe mempty (pure . flip SetLED True) (modeLED initialMode)
                    )
                . S.cons [LogEvent "Starting..."]
                . S.morphInner (lift . lift . lift)
                $ S.parList
                    id
                    [ scanStream
                        (KeyboardState mempty initialMode Normal False False False Nothing Nothing)
                        ( uncurry \d ->
                            runStateT
                                . either
                                    ( either
                                        ( \() -> do
                                            logMessage $ "Evdev device added: " <> decodeUtf8 (Evdev.devicePath d)
                                            #keyboards %= Set.insert d
                                            -- TODO unify "always grabbed unless in Idle mode" logic somewhere?
                                            mode <- use #mode
                                            pure $ mwhen (mode /= Idle) [ActionEvent mempty $ send $ EvdevGrab d]
                                        )
                                        ( \e -> do
                                            logMessage $ "Evdev device removed: " <> showT e
                                            #keyboards %= Set.delete d
                                            pure []
                                        )
                                    )
                                    (state . dispatchKeys (opts & \Opts{..} -> KeyboardOpts{..}) . Evdev.eventData)
                        )
                        -- I can't find a reliable heuristic for "basically a keyboard" so we filter by name
                        . S.filterM (fmap ((`elem` opts.keyboard) . decodeUtf8) . liftIO . Evdev.deviceName . fst)
                        . readEventsMany
                        . S.mapMaybeM
                            ( either
                                ( \(p, e) ->
                                    logMessage
                                        ("Couldn't create evdev device from " <> decodeUtf8 p <> ": " <> showT e)
                                        >> pure Nothing
                                )
                                (pure . Just)
                            )
                        . S.append allDevices
                        $ newDevices' 1_000_000
                    , S.repeatM $ pure <$> liftIO (takeMVar eventMVar)
                    ]

data Event where
    ActionEvent :: (Show a) => (a -> IO ()) -> (CompoundAction a) -> Event
    LogEvent :: Text -> Event
    ErrorEvent :: Error -> Event

type CompoundAction a = Eff '[Action] a

data Action a where
    Exit :: Action ()
    ResetError :: Action ()
    Sleep :: NominalDiffTime -> Action ()
    SetLED :: Int -> Bool -> Action ()
    SetSystemLEDs :: Bool -> Action ()
    EvdevGrab :: Evdev.Device -> Action ()
    EvdevUngrab :: Evdev.Device -> Action ()
    SendKey :: Key -> KeyEvent -> Action ()
    GetCurrentLight :: Action Lifx.Device
    LightReScan :: Action ()
    NextLight :: Action ()
    GetLightPower :: Lifx.Device -> Action Bool
    SetLightPower :: Lifx.Device -> Bool -> Action ()
    UnsetLightColourCache :: Action ()
    GetLightColour :: Bool -> Lifx.Device -> Action HSBK
    SetLightColour :: Bool -> Lifx.Device -> NominalDiffTime -> HSBK -> Action ()
    GetLightState :: Lifx.Device -> Action Lifx.LightState
    GetLightName :: Lifx.Device -> Action Text
    Mpris :: Text -> Action ()
    SendIR :: IRCmdType -> IRDev -> Text -> Action ()
    ToggleHifiPlug :: Action ()
    SpotifySearchAndPlay :: Spotify.SearchType -> Text -> Action ()
deriving instance Show (Action a)
data IRDev
    = IRHifi
    | IRTV -- TODO move to separate module to avoid need for prefixes?
    | IRSwitcher
    deriving (Show)
data IRCmdType
    = IRStart
    | IRStop
    | IROnce
    deriving (Show)

data ActionOpts = ActionOpts
    { ledErrorPin :: Int
    , setLED :: forall m. (MonadState AppState m, MonadLog Text m, MonadIO m) => Int -> Bool -> m ()
    , spotifyDeviceId :: Spotify.DeviceID
    , keySendPort :: PortNumber
    , keySendIps :: [IP]
    }
runAction ::
    forall m a.
    (MonadIO m, MonadCatch m, MonadState AppState m, MonadLifx m, MonadLog Text m, MonadError Error m) =>
    ActionOpts ->
    Action a ->
    m a
runAction opts@ActionOpts{setLED {- TODO GHC doesn't yet support impredicative fields -}} = (.) catchIO \case
    Exit -> liftIO exitSuccess
    ResetError -> setLED opts.ledErrorPin False
    Sleep t -> liftIO $ threadDelay' t
    SetLED n b -> setLED n b
    SetSystemLEDs b ->
        traverse_
            (\(l, v) -> liftIO $ readProcess "sudo" ["tee", "/sys/class/leds/" <> l <> "/trigger"] (v <> "\n"))
            (if b then [("ACT", "mmc0"), ("PWR", "default-on")] else [("ACT", "none"), ("PWR", "none")])
    EvdevGrab d -> liftIO $ Evdev.grabDevice d
    EvdevUngrab d -> liftIO $ Evdev.ungrabDevice d
    SendKey k e -> do
        -- TODO DRY this with my `net-evdev` repo
        sock <- use #keySendSocket
        liftIO . for_ opts.keySendIps $
            void
                . sendTo sock (B.pack [fromIntegral $ fromEnum k, fromIntegral $ fromEnum e])
                . (SockAddrInet opts.keySendPort . (.unIP))
    GetCurrentLight -> fst . Stream.head <$> use #bulbs
    LightReScan ->
        maybe
            (logMessage "No LIFX devices found during re-scan - retaining old list")
            (\ds -> #bulbs .= Stream.cycle ds >> logMessage ("LIFX devices found: " <> showT (toList ds)))
            . nonEmpty
            =<< discoverLifx
    NextLight -> #bulbs %= Stream.tail
    GetLightPower l -> statePowerToBool <$> Lifx.sendMessage l Lifx.GetPower
    SetLightPower l p -> Lifx.sendMessage l $ Lifx.SetPower p
    UnsetLightColourCache -> #lightColourCache .= Nothing
    GetLightColour useCache l ->
        if useCache
            then maybe (throwError $ SimpleError "Light colour cache is empty") pure =<< use #lightColourCache
            else (.hsbk) <$> Lifx.sendMessage l Lifx.GetColor
    SetLightColour setCache l d c -> do
        when setCache $ #lightColourCache ?= c
        Lifx.sendMessage l $ Lifx.SetColor c d
    GetLightState l -> Lifx.sendMessage l Lifx.GetColor
    GetLightName l -> (.label) <$> Lifx.sendMessage l Lifx.GetColor
    Mpris cmd -> do
        service <-
            maybe
                (throwError $ SimpleError "Failed to find spotifyd in qdbus output")
                (pure . dropWhile isSpace)
                . find (" org.mpris.MediaPlayer2.spotifyd" `isPrefixOf`)
                . lines
                =<< liftIO (readProcess "qdbus" [] "")
        liftIO . void $
            readProcess
                "dbus-send"
                [ "--print-reply"
                , "--dest=" <> service
                , "/org/mpris/MediaPlayer2"
                , "org.mpris.MediaPlayer2.Player." <> T.unpack cmd
                ]
                ""
    SendIR type_ dev cmd ->
        liftIO . void $
            readProcess
                "irsend"
                ( map
                    T.unpack
                    [ case type_ of
                        IRStart -> "SEND_START"
                        IRStop -> "SEND_STOP"
                        IROnce -> "SEND_ONCE"
                    , case dev of
                        IRHifi -> "rak-ch215wh"
                        IRTV -> "LG-AKB74475403"
                        IRSwitcher -> "PROZOR-B073TXX3KV"
                    , cmd
                    ]
                )
                ""
    ToggleHifiPlug -> do
        man <- use #httpConnectionManager
        response <- liftIO $ flip httpLbs man =<< parseRequest "http://192.168.1.114/rpc/Switch.Toggle?id=0"
        logMessage $ "HTTP response status code from HiFi plug: " <> showT (statusCode $ responseStatus response)
    SpotifySearchAndPlay searchType query -> do
        searchResult <- liftIO $ Spotify.search query [searchType] Nothing Nothing Spotify.noPagingParams
        case searchType of
            Spotify.AlbumSearch -> do
                album <- getURI searchResult.albums
                play (Just album) Nothing
            Spotify.ArtistSearch -> do
                -- TODO shuffle my liked songs by artist instead of playing top ones globally?
                artist <- getURI searchResult.artists
                play (Just artist) Nothing
            Spotify.PlaylistSearch -> do
                playlist <- getURI searchResult.playlists
                play (Just playlist) Nothing
            Spotify.TrackSearch -> do
                track <- getURI searchResult.tracks
                play Nothing (Just [track])
            -- TODO improve library to support all search types properly
            t -> throwError $ Error "Unsupported Spotify search type" t
      where
        getURI :: (HasField "uri" x Spotify.URI) => Maybe (Spotify.Paging x) -> m Spotify.URI
        getURI =
            maybe (throwError $ Error "No Spotify entries" (searchType, query)) pure
                . (fmap (.uri) . listToMaybe . (.items) =<<)
        play context item =
            liftIO
                . Spotify.startPlayback (Just opts.spotifyDeviceId)
                $ Spotify.StartPlaybackOpts context item Nothing

data KeyboardOpts = KeyboardOpts
    { flashTime :: NominalDiffTime
    , modeLED :: Mode -> Maybe Int
    }
    deriving (Generic)
data KeyboardState = KeyboardState
    { keyboards :: Set Evdev.Device
    , mode :: Mode
    , previousMode :: Mode
    , shift :: Bool
    , ctrl :: Bool
    , alt :: Bool
    , modeChangeState :: Maybe (Maybe Key)
    , typing :: Maybe (TypingReason, [Char])
    }
    deriving (Generic)
newtype TypingReason
    = TypingSpotifySearch Spotify.SearchType
dispatchKeys :: KeyboardOpts -> Evdev.EventData -> KeyboardState -> ([Event], KeyboardState)
dispatchKeys opts event ks0@KeyboardState{..} = second (setMods . ($ ks0)) case event of
    KeyEvent KeyL Pressed | ctrl && shift -> startSpotifySearch Spotify.AlbumSearch
    KeyEvent KeyA Pressed | ctrl && shift -> startSpotifySearch Spotify.ArtistSearch
    KeyEvent KeyP Pressed | ctrl && shift -> startSpotifySearch Spotify.PlaylistSearch
    KeyEvent KeyS Pressed | ctrl && shift -> startSpotifySearch Spotify.TrackSearch
    KeyEvent KeyW Pressed | ctrl && shift -> startSpotifySearch Spotify.ShowSearch
    KeyEvent KeyE Pressed | ctrl && shift -> startSpotifySearch Spotify.EpisodeSearch
    KeyEvent KeyB Pressed | ctrl && shift -> startSpotifySearch Spotify.AudiobookSearch
    KeyEvent KeyEsc Pressed | Just _ <- typing -> ([LogEvent "Discarding keyboard input"], #typing .~ Nothing)
    KeyEvent KeyEnter Pressed | Just (t, cs) <- typing -> (,#typing .~ Nothing) case t of
        TypingSpotifySearch searchType -> act $ send $ SpotifySearchAndPlay searchType text
          where
            -- TODO why can't I de-indent this where? GHC bug?
            text = T.pack $ reverse cs
    KeyEvent k e | Just (t, cs) <- typing -> case e of
        Pressed -> case keyToChar shift k of
            Just c -> (mempty, #typing ?~ (t, c : cs))
            Nothing ->
                ( [LogEvent $ "Ignoring non-character keypress" <> mwhen shift " (with shift)" <> ": " <> showT k]
                , id
                )
        _ -> (mempty, id)
    _ | Just mk <- modeChangeState -> case event of
        KeyEvent KeyRightalt Released -> second ((#modeChangeState .~ Nothing) .) case mk of
            Nothing -> f previousMode
            Just k -> case k of
                KeyEsc -> f Idle
                KeyQ -> f Quiet
                KeyDot -> f Normal
                KeyT -> f TV
                KeyComma -> f Sending
                _ -> ([LogEvent $ "Key does not correspond to any mode: " <> showT k], id)
          where
            old = mode
            f new =
                (
                    [ LogEvent $ "Changing keyboard mode: " <> showT new
                    , ActionEvent mempty do
                        for_ (opts.modeLED old) $ send . flip SetLED False
                        for_ (opts.modeLED new) $ send . flip SetLED True
                        case old of
                            Idle -> traverse_ (send . EvdevGrab) keyboards
                            Quiet -> send $ SetSystemLEDs True
                            _ -> pure ()
                        case new of
                            Idle -> traverse_ (send . EvdevUngrab) keyboards
                            Quiet -> send $ SetSystemLEDs False
                            _ -> pure ()
                    ]
                , (#mode .~ new) . (#previousMode .~ old)
                )
        _ -> (mempty,) case event of
            KeyEvent k e | (k, e) /= (KeyRightalt, Repeated) -> #modeChangeState ?~ Just k
            _ -> id
    _ -> (,id) case mode of
        Idle -> []
        Quiet -> []
        Sending -> case event of
            KeyEvent k e | k /= KeyRightalt -> simpleAct $ SendKey k e
            _ -> []
        Normal -> case event of
            KeyEvent KeyEsc Pressed | ctrl -> simpleAct Exit
            KeyEvent KeyR Pressed | ctrl -> simpleAct ResetError
            KeyEvent KeyP Pressed ->
                if ctrl
                    then simpleAct ToggleHifiPlug
                    else act do
                        send $ SendIR IROnce IRHifi "KEY_POWER"
                        send $ Sleep 1
                        send $ SendIR IROnce IRHifi "KEY_TAPE"
            KeyEvent KeyS Pressed -> act do
                send NextLight
                l <- send GetCurrentLight
                Lifx.LightState{power = (== 0) -> wasOff, ..} <- send $ GetLightState l
                when wasOff $ send $ SetLightPower l True
                send $ SetLightColour False l opts.flashTime $ hsbk & #brightness %~ (`div` 2)
                send $ Sleep opts.flashTime
                send $ SetLightColour False l opts.flashTime hsbk
                when wasOff $ send $ SetLightPower l False
            KeyEvent KeyVolumeup e -> irHold e IRHifi "KEY_VOLUMEUP"
            KeyEvent KeyVolumedown e -> irHold e IRHifi "KEY_VOLUMEDOWN"
            KeyEvent KeyMute Pressed -> irOnce IRHifi "muting"
            KeyEvent KeyPlaypause Pressed -> simpleAct $ Mpris "PlayPause"
            KeyEvent KeyPrevioussong Pressed -> simpleAct $ Mpris "Previous"
            KeyEvent KeyNextsong Pressed -> simpleAct $ Mpris "Next"
            KeyEvent KeyR Pressed -> simpleAct LightReScan
            KeyEvent KeyL Pressed -> act do
                l <- send GetCurrentLight
                p <- send $ GetLightPower l
                send $ SetLightPower l $ not p
            KeyEvent KeyLeft e -> modifyLight e $ #hue %~ subtract hueInterval
            KeyEvent KeyRight e -> modifyLight e $ #hue %~ (+ hueInterval)
            KeyEvent KeyMinus e -> modifyLight e $ #saturation %~ incrementLightField clampedSub minBound 256
            KeyEvent KeyEqual e -> modifyLight e $ #saturation %~ incrementLightField clampedAdd maxBound 256
            KeyEvent KeyDown e -> modifyLight e $ #brightness %~ incrementLightField clampedSub minBound 256
            KeyEvent KeyUp e -> modifyLight e $ #brightness %~ incrementLightField clampedAdd maxBound 256
            KeyEvent KeyLeftbrace e -> modifyLight e $ #kelvin %~ incrementLightField clampedSub 1500 25
            KeyEvent KeyRightbrace e -> modifyLight e $ #kelvin %~ incrementLightField clampedAdd 9000 25
            _ -> []
        TV -> case event of
            KeyEvent KeySpace Pressed -> act do
                send $ SendIR IROnce IRTV "KEY_AUX"
                send $ Sleep t
                send $ SendIR IROnce IRTV "KEY_AUX"
                send $ Sleep t
                send $ SendIR IROnce IRTV "KEY_OK"
              where
                t = if ctrl then 1 else 0.35
            KeyEvent KeyP e -> irHold e IRTV "KEY_POWER"
            KeyEvent Key1 e -> irHold e (if ctrl then IRSwitcher else IRTV) "KEY_1"
            KeyEvent Key2 e -> irHold e (if ctrl then IRSwitcher else IRTV) "KEY_2"
            KeyEvent Key3 e -> irHold e (if ctrl then IRSwitcher else IRTV) "KEY_3"
            KeyEvent Key4 e -> irHold e IRTV "KEY_4"
            KeyEvent Key5 e -> irHold e IRTV "KEY_5"
            KeyEvent Key6 e -> irHold e IRTV "KEY_6"
            KeyEvent Key7 e -> irHold e IRTV "KEY_7"
            KeyEvent Key8 e -> irHold e IRTV "KEY_8"
            KeyEvent Key9 e -> irHold e IRTV "KEY_9"
            KeyEvent Key0 e -> irHold e IRTV "KEY_0"
            KeyEvent KeyVolumeup e -> irHold e IRTV "KEY_VOLUMEUP"
            KeyEvent KeyVolumedown e -> irHold e IRTV "KEY_VOLUMEDOWN"
            KeyEvent KeyMute e -> irHold e IRTV "KEY_MUTE"
            KeyEvent KeyComma e -> irHold e IRTV "KEY_CHANNELDOWN"
            KeyEvent KeyDot e -> irHold e IRTV "KEY_CHANNELUP"
            KeyEvent KeyA e -> irHold e IRTV "KEY_AUX"
            KeyEvent KeyS e -> irHold e IRTV "KEY_SETUP"
            KeyEvent KeyR e -> irHold e IRTV "KEY_RED"
            KeyEvent KeyT e -> irHold e IRTV "KEY_SUBTITLE"
            KeyEvent KeyG e -> irHold e IRTV "KEY_G"
            KeyEvent KeyQ e -> irHold e IRTV "KEY_MENU"
            KeyEvent KeyUp e -> irHold e IRTV "KEY_UP"
            KeyEvent KeyDown e -> irHold e IRTV "KEY_DOWN"
            KeyEvent KeyLeft e -> irHold e IRTV "KEY_LEFT"
            KeyEvent KeyRight e -> irHold e IRTV "KEY_RIGHT"
            KeyEvent KeyEnter e -> irHold e IRTV "KEY_OK"
            KeyEvent KeyBackspace e -> irHold e IRTV "KEY_BACK"
            KeyEvent KeyI e -> irHold e IRTV "KEY_INFO"
            KeyEvent KeyEsc e -> irHold e IRTV "KEY_EXIT"
            _ -> []
  where
    setMods = case event of
        KeyEvent KeyLeftctrl e -> setMod #ctrl e
        KeyEvent KeyRightctrl e -> setMod #ctrl e
        KeyEvent KeyLeftshift e -> setMod #shift e
        KeyEvent KeyRightshift e -> setMod #shift e
        KeyEvent KeyLeftalt e -> setMod #alt e
        KeyEvent KeyRightalt Pressed -> #modeChangeState ?~ Nothing
        _ -> id
      where
        setMod l = \case
            Pressed -> l .~ True
            Released -> l .~ False
            Repeated -> id
    simpleAct = act . send
    act = pure . ActionEvent mempty
    irOnce = simpleAct .: SendIR IROnce
    irHold = \case
        Pressed -> simpleAct .: SendIR IRStart
        Repeated -> mempty
        Released -> simpleAct .: SendIR IRStop
    hueInterval = 16 * if ctrl then 16 else if shift then 4 else 1
    clampedAdd m a b = b + min (m - b) a -- TODO better implementation? maybe in library? else, this is presumably commutative in last two args (ditto below)
    clampedSub m a b = b - min (b - m) a
    modifyLight e f = act case e of
        Pressed -> setColour False =<< send GetCurrentLight
        Repeated -> setColour True =<< send GetCurrentLight
        Released -> send UnsetLightColourCache
      where
        setColour useCache l = send . SetLightColour True l 0 . f =<< send (GetLightColour useCache l)
    incrementLightField f bound inc = if ctrl then const bound else f bound if shift then inc * 4 else inc
    startSpotifySearch t = ([LogEvent "Waiting for keyboard input"], #typing ?~ (TypingSpotifySearch t, []))

-- we only use this for actions which return a response
webServer :: (forall m. (MonadIO m) => Event -> m ()) -> Wai.Application
webServer f =
    makeOkapiApp id $
        asum
            [ withGetRoute "light" $ f' id (send . GetLightName =<< send GetCurrentLight)
            ]
  where
    withGetRoute s x = Okapi.get >> seg s >> x
    f' :: (Show a) => (a -> Text) -> CompoundAction a -> OkapiT IO Result
    f' show' x = do
        m <- liftIO newEmptyMVar
        f $ ActionEvent (putMVar m) x
        okPlainText [] . (<> "\n") . show' =<< liftIO (takeMVar m)
