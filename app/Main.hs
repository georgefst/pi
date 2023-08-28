module Main (main) where

import GPIO qualified
import Util
import Util.Lifx

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
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
import Data.Stream.Infinite qualified as Stream
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time
import Data.Tuple.Extra
import Data.Word
import Evdev (EventData (KeyEvent), KeyEvent (..))
import Evdev qualified
import Evdev.Codes (Key (..))
import Lifx.Lan hiding (SetLightPower)
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString hiding (send)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi hiding (Event, error, get)
import Okapi qualified
import Optics
import Optics.State.Operators
import Options.Generic
import Streamly.Data.Fold qualified as SF
import Streamly.Data.Stream qualified as S
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.Stream.StreamK qualified as SK (hoist)
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
    , keySendPort :: PortNumber
    , keySendIps :: [IP]
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

data AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    , lifxDevices :: Stream.Stream (Device, LightState)
    , httpConnectionManager :: Manager
    , keySendSocket :: Socket
    , mode :: Mode
    , previousMode :: Mode
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

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Pi"
    eventMVar <- newEmptyMVar

    let
        setLED :: (MonadState AppState m, MonadIO m) => Int -> Bool -> m ()
        setLED pin =
            bool
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Just h -> GPIO.reset h >> modifying #activeLEDs (Map.delete pin)
                    Nothing -> log' $ "LED is already off: " <> showT pin
                )
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Nothing -> GPIO.set opts.gpioChip [pin] >>= modifying #activeLEDs . Map.insert pin
                    Just _ -> log' $ "LED is already on: " <> showT pin
                )
          where
            log' = liftIO . putMVar eventMVar . LogEvent

        handleError :: (MonadIO m, MonadState AppState m) => Error -> m ()
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
            =<< runLifxT (lifxTime opts.lifxTimeout) (Just $ fromIntegral opts.lifxPort * 2) discoverLifx
    let initialState =
            AppState
                { activeLEDs = mempty
                , lifxDevices = Stream.cycle ds
                , mode = Idle
                , previousMode = Normal
                , lightColourCache = Nothing
                , ..
                }

    race_
        ( Warp.runSettings
            (warpSettings opts.httpPort $ putMVar eventMVar . ErrorEvent . Error "HTTP error")
            (webServer $ liftIO . putMVar eventMVar)
            -- TODO disabled - see `gpioMonitor` definition
            -- `race_` gpioMonitor
        )
        . flip evalStateT initialState
        . flip runLoggingT (liftIO . T.putStrLn)
        . runLifxUntilSuccess
            (either (handleError . Error @() "Misc exception") (handleError . Error "LIFX error"))
            (lifxTime opts.lifxTimeout)
            (Just $ fromIntegral opts.lifxPort)
        $ do
            keyboard <- liftIO $ Evdev.newDevice "/dev/input/event3"
            S.fold
                ( SF.drainMapM \case
                    ErrorEvent e -> handleError e
                    LogEvent t -> logMessage t
                    ActionEvent f action ->
                        (either handleError pure <=< runExceptT)
                            . runM
                            . (sendM . (liftIO . f) <=< translate (runAction $ opts & \Opts{..} -> ActionOpts{..}))
                            $ action
                )
                . S.mapMaybeM gets
                . (SK.toStream . SK.hoist liftIO . SK.fromStream)
                . S.append
                    ( -- flash all lights to show we have finished initialising
                      S.fromList
                        . map (const . Just . ActionEvent mempty . send)
                        . intersperse (Sleep 0.4)
                        -- TODO we shouldn't have to actually set the mode - I only really want to flash the LED
                        -- but then there's no real harm, except that and that we have to repeat the initial states
                        -- and we don't display the red LED
                        -- and we'd quite like to set them all off briefly to make it clearer we've finished
                        -- anyway, easy to solve - just split apart `SetMode` so we have separate action for LEDs
                        . map SetMode
                        $ enumerate <> [initialState.previousMode, initialState.mode]
                    )
                . S.cons (const $ Just $ LogEvent "Starting...")
                $ S.parList
                    id
                    [ scanStream
                        (KeyboardState False False False Nothing)
                        (dispatchKeys $ opts & \Opts{..} -> KeyboardOpts{..})
                        . S.repeatM
                        $ Evdev.eventData <$> Evdev.nextEvent keyboard -- TODO use `evdev-streamly`
                    , S.repeatM $ const . Just <$> takeMVar eventMVar
                    ]

data Event where
    ActionEvent :: (a -> IO ()) -> (CompoundAction a) -> Event
    LogEvent :: Text -> Event
    ErrorEvent :: Error -> Event

type CompoundAction a = Eff '[Action] a

data Action a where
    Exit :: Action ()
    SetMode :: Mode -> Action ()
    ResetError :: Action ()
    Sleep :: NominalDiffTime -> Action ()
    SendKey :: Key -> KeyEvent -> Action ()
    GetCurrentLight :: Action Device
    GetLightColourCache :: Action (Maybe HSBK)
    SetLightColourCache :: HSBK -> Action ()
    UnsetLightColourCache :: Action ()
    NextLight :: Action ()
    GetLightPower :: Device -> Action Bool
    SetLightPower :: Device -> Bool -> Action ()
    GetLightColour :: Device -> Action HSBK
    SetLightColour :: Device -> NominalDiffTime -> HSBK -> Action ()
    GetLightState :: Device -> Action LightState
    GetLightName :: Device -> Action Text
    Mpris :: Text -> Action ()
    SendIR :: IRCmdType -> IRDev -> Text -> Action ()
    ToggleHifiPlug :: Action ()
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
    , keyboard :: Evdev.Device
    , modeLED :: Mode -> Maybe Int
    , setLED :: forall m. (MonadState AppState m, MonadIO m) => Int -> Bool -> m ()
    , keySendPort :: PortNumber
    , keySendIps :: [IP]
    }

runAction ::
    (MonadIO m, MonadState AppState m, MonadLifx m, MonadLog Text m, MonadError Error m) =>
    ActionOpts ->
    Action a ->
    m a
runAction opts@ActionOpts{setLED {- TODO GHC doesn't yet support impredicative fields -}} = \case
    Exit -> liftIO exitSuccess
    SetMode new -> do
        old <- use #mode
        #mode .= new
        #previousMode .= old
        for_ (opts.modeLED old) $ flip setLED False
        for_ (opts.modeLED new) $ flip setLED True
        case old of
            Idle -> liftIO $ Evdev.grabDevice opts.keyboard
            Quiet -> setSystemLEDs [("ACT", "mmc0"), ("PWR", "default-on")]
            _ -> pure ()
        case new of
            Idle -> liftIO $ Evdev.ungrabDevice opts.keyboard
            Quiet -> setSystemLEDs [("ACT", "none"), ("PWR", "none")]
            _ -> pure ()
      where
        setSystemLEDs = traverse_ \(l, v) ->
            liftIO $ readProcess "sudo" ["tee", "/sys/class/leds/" <> l <> "/trigger"] (v <> "\n")
    ResetError -> setLED opts.ledErrorPin False
    Sleep t -> liftIO $ threadDelay' t
    SendKey k e -> do
        -- TODO DRY this with my `net-evdev` repo
        sock <- use #keySendSocket
        liftIO . for_ opts.keySendIps $
            void
                . sendTo sock (B.pack [fromIntegral $ fromEnum k, fromIntegral $ fromEnum e])
                . (SockAddrInet opts.keySendPort . (.unIP))
    GetCurrentLight -> fst . Stream.head <$> use #lifxDevices
    GetLightColourCache -> use #lightColourCache
    SetLightColourCache l -> #lightColourCache ?= l
    UnsetLightColourCache -> #lightColourCache .= Nothing
    NextLight -> #lifxDevices %= Stream.tail
    GetLightPower l -> statePowerToBool <$> sendMessage l GetPower
    SetLightPower l p -> sendMessage l $ SetPower p
    GetLightColour l -> (.hsbk) <$> sendMessage l GetColor
    SetLightColour l d c -> sendMessage l $ SetColor c d
    GetLightState l -> sendMessage l GetColor
    GetLightName l -> (.label) <$> sendMessage l GetColor
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

newtype KeyboardOpts = KeyboardOpts
    { flashTime :: NominalDiffTime
    }
    deriving (Generic)
data KeyboardState = KeyboardState
    { shift :: Bool
    , ctrl :: Bool
    , alt :: Bool
    , modeChangeState :: Maybe (Maybe Key)
    }
    deriving (Generic)
dispatchKeys :: KeyboardOpts -> Evdev.EventData -> KeyboardState -> (AppState -> Maybe Event, KeyboardState)
dispatchKeys opts event s@KeyboardState{..} = case modeChangeState of
    Just mk -> case event of
        KeyEvent KeyRightalt Released -> (,s & #modeChangeState .~ Nothing) case mk of
            Nothing -> \AppState{..} -> simpleAct $ SetMode previousMode
            Just k -> const case k of
                KeyEsc -> simpleAct $ SetMode Idle
                KeyQ -> simpleAct $ SetMode Quiet
                KeyDot -> simpleAct $ SetMode Normal
                KeyT -> simpleAct $ SetMode TV
                KeyComma -> simpleAct $ SetMode Sending
                _ -> Just $ LogEvent $ "Key does not correspond to any mode: " <> showT k
        _ ->
            ( const Nothing
            , s & case event of
                KeyEvent k e | (k, e) /= (KeyRightalt, Repeated) -> #modeChangeState ?~ Just k
                _ -> id
            )
    Nothing -> (,s & case event of
                    KeyEvent KeyLeftctrl e -> setMod #ctrl e
                    KeyEvent KeyRightctrl e -> setMod #ctrl e
                    KeyEvent KeyLeftshift e -> setMod #shift e
                    KeyEvent KeyRightshift e -> setMod #shift e
                    KeyEvent KeyLeftalt e -> setMod #alt e
                    KeyEvent KeyRightalt Pressed -> #modeChangeState ?~ Nothing
                    _ -> id)
        \AppState{..} -> case mode of
            Idle -> Nothing
            Quiet -> Nothing
            Sending -> case event of
                KeyEvent k e | k /= KeyRightalt -> simpleAct $ SendKey k e
                _ -> Nothing
            Normal -> case event of
                KeyEvent KeyEsc Pressed | ctrl -> simpleAct Exit
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
                    LightState{power = (== 0) -> wasOff, ..} <- send $ GetLightState l
                    when wasOff $ send $ SetLightPower l True
                    send $ SetLightColour l opts.flashTime $ hsbk & #brightness %~ (`div` 2)
                    send $ Sleep opts.flashTime
                    send $ SetLightColour l opts.flashTime hsbk
                    when wasOff $ send $ SetLightPower l False
                KeyEvent KeyVolumeup e -> irHold e IRHifi "KEY_VOLUMEUP"
                KeyEvent KeyVolumedown e -> irHold e IRHifi "KEY_VOLUMEDOWN"
                KeyEvent KeyMute Pressed -> irOnce IRHifi "muting"
                KeyEvent KeyPlaypause Pressed -> simpleAct $ Mpris "PlayPause"
                KeyEvent KeyPrevioussong Pressed -> simpleAct $ Mpris "Previous"
                KeyEvent KeyNextsong Pressed -> simpleAct $ Mpris "Next"
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
                _ -> Nothing
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
                _ -> Nothing
  where
    setMod l = \case
        Pressed -> l .~ True
        Released -> l .~ False
        Repeated -> id
    simpleAct = act . send
    act = Just . ActionEvent mempty
    irOnce = simpleAct .: SendIR IROnce
    irHold = \case
        Pressed -> simpleAct .: SendIR IRStart
        Repeated -> const $ const Nothing
        Released -> simpleAct .: SendIR IRStop
    hueInterval = 16 * if ctrl then 16 else if shift then 4 else 1
    clampedAdd m a b = b + min (m - b) a -- TODO better implementation? maybe in library? else, this is presumably commutative in last two args (ditto below)
    clampedSub m a b = b - min (b - m) a
    modifyLight e f = act do
        l <- send GetCurrentLight
        case e of
            Pressed -> cacheAndSet l . f =<< send (GetLightColour l)
            -- the `Nothing` case here shouldn't actually happen - we can assume cache is set by last key event
            Repeated -> cacheAndSet l . f =<< maybe (send $ GetLightColour l) pure =<< send GetLightColourCache
            Released -> send UnsetLightColourCache
      where
        cacheAndSet l c = do
            send $ SetLightColourCache c
            send $ SetLightColour l 0 c
    incrementLightField f bound inc = if ctrl then const bound else f bound if shift then inc * 4 else inc

-- we only use this for actions which return a response
webServer :: (forall m. (MonadIO m) => Event -> m ()) -> Wai.Application
webServer f =
    makeOkapiApp id $
        asum
            [ withGetRoute "light" $ f' id (send . GetLightName =<< send GetCurrentLight)
            ]
  where
    withGetRoute s x = Okapi.get >> seg s >> x
    f' :: (a -> Text) -> CompoundAction a -> OkapiT IO Result
    f' show' x = do
        m <- liftIO newEmptyMVar
        f $ ActionEvent (putMVar m) x
        okPlainText [] . (<> "\n") . show' =<< liftIO (takeMVar m)

warpSettings ::
    Warp.Port ->
    (forall a. (Show a) => a -> IO ()) ->
    Warp.Settings
warpSettings port logError =
    Warp.setLogger (curry3 $ unless . statusIsSuccessful . snd3 <*> logError)
        . Warp.setPort port
        $ Warp.defaultSettings
