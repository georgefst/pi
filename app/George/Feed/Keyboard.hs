{- HLINT ignore "Redundant section" -}
module George.Feed.Keyboard (feed, Opts (..), Mode (..)) where

import George.Core
import Util

import Control.Monad
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage)
import Control.Monad.State.Strict
import Data.Foldable
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding
import Evdev (EventData (KeyEvent), KeyEvent (..))
import Evdev qualified
import Evdev.Codes (Key (..))
import Evdev.Stream
import Lifx.Lan qualified as Lifx
import Optics
import Optics.State.Operators
import Options.Generic
import Spotify.Types.Search qualified as Spotify
import Streamly.Data.Stream.Prelude qualified as S

newtype Opts = Opts
    { modeLED :: Mode -> Maybe Int
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
data Mode
    = Idle -- let the OS handle keypresses
    | Quiet -- just turn off LEDs
    | Sending -- send keypresses over UDP
    | Normal
    | TV
    deriving (Eq, Ord, Show, Enum, Bounded)

newtype TypingReason
    = TypingSpotifySearch Spotify.SearchType

dispatchKeys :: (MonadIO m) => Opts -> Evdev.EventData -> KeyboardState -> m ([Event], KeyboardState)
dispatchKeys opts event = wrap \KeyboardState{..} -> case (typing, modeChangeState) of
    (Just (t, cs), _) -> case event of
        KeyEvent KeyEsc Pressed -> #typing .= Nothing >> pure [LogEvent "Discarding keyboard input"]
        KeyEvent KeyEnter Pressed -> (#typing .= Nothing >>) case t of
            TypingSpotifySearch searchType -> act $ send $ SpotifySearchAndPlay searchType text
              where
                -- TODO why can't I de-indent this where? GHC bug?
                text = T.pack $ reverse cs
        KeyEvent KeyBackspace Pressed -> #typing ?= (t, tailSafe cs) >> pure []
        KeyEvent k Pressed -> case keyToChar shift k of
            Just c -> #typing ?= (t, c : cs) >> pure []
            Nothing ->
                pure [LogEvent $ "Ignoring non-character keypress" <> mwhen shift " (with shift)" <> ": " <> showT k]
        _ -> pure []
    (_, Just mk) -> case event of
        KeyEvent KeyRightalt Released -> (#modeChangeState .= Nothing >>) case mk of
            Nothing -> f previousMode
            Just k -> case k of
                KeyEsc -> f Idle
                KeyQ -> f Quiet
                KeyDot -> f Normal
                KeyT -> f TV
                KeyComma -> f Sending
                _ -> pure [LogEvent $ "Key does not correspond to any mode: " <> showT k]
          where
            old = mode
            f new = do
                #mode .= new
                #previousMode .= old
                when (old == Idle) $ traverse_ (liftIO . Evdev.grabDevice) keyboards
                when (new == Idle) $ traverse_ (liftIO . Evdev.ungrabDevice) keyboards
                pure
                    [ LogEvent $ "Changing keyboard mode: " <> showT new
                    , ActionEvent mempty do
                        for_ (opts.modeLED old) $ send . flip SetLED False
                        for_ (opts.modeLED new) $ send . flip SetLED True
                        case old of
                            Quiet -> send $ SetSystemLEDs True
                            _ -> pure ()
                        case new of
                            Quiet -> send $ SetSystemLEDs False
                            _ -> pure ()
                    ]
        KeyEvent k e | (k, e) /= (KeyRightalt, Repeated) -> #modeChangeState ?= Just k >> pure []
        _ -> pure []
    (Nothing, Nothing) -> case mode of
        Idle -> pure []
        Quiet -> pure []
        Sending -> case event of
            KeyEvent k e | k /= KeyRightalt -> simpleAct $ SendKey k e
            _ -> pure []
        Normal -> case event of
            KeyEvent KeyEsc Pressed | ctrl -> simpleAct Exit
            KeyEvent KeyR Pressed | ctrl -> simpleAct ResetError
            KeyEvent KeyL Pressed | ctrl && shift -> startSpotifySearch Spotify.AlbumSearch
            KeyEvent KeyA Pressed | ctrl && shift -> startSpotifySearch Spotify.ArtistSearch
            KeyEvent KeyP Pressed | ctrl && shift -> startSpotifySearch Spotify.PlaylistSearch
            KeyEvent KeyS Pressed | ctrl && shift -> startSpotifySearch Spotify.TrackSearch
            KeyEvent KeyW Pressed | ctrl && shift -> startSpotifySearch Spotify.ShowSearch
            KeyEvent KeyE Pressed | ctrl && shift -> startSpotifySearch Spotify.EpisodeSearch
            KeyEvent KeyB Pressed | ctrl && shift -> startSpotifySearch Spotify.AudiobookSearch
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
                send $ SetLightColour False l flashTime $ hsbk & #brightness %~ (`div` 2)
                send $ Sleep flashTime
                send $ SetLightColour False l flashTime hsbk
                when wasOff $ send $ SetLightPower l False
              where
                flashTime = 0.35
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
            KeyEvent KeyLeft e -> modifyLight e $ #hue %~ subtract (hueInterval ctrl shift)
            KeyEvent KeyRight e -> modifyLight e $ #hue %~ (+ hueInterval ctrl shift)
            KeyEvent KeyMinus e -> modifyLight e $ #saturation %~ incrementLightField ctrl shift clampedSub minBound 256
            KeyEvent KeyEqual e -> modifyLight e $ #saturation %~ incrementLightField ctrl shift clampedAdd maxBound 256
            KeyEvent KeyDown e -> modifyLight e $ #brightness %~ incrementLightField ctrl shift clampedSub minBound 256
            KeyEvent KeyUp e -> modifyLight e $ #brightness %~ incrementLightField ctrl shift clampedAdd maxBound 256
            KeyEvent KeyLeftbrace e -> modifyLight e $ #kelvin %~ incrementLightField ctrl shift clampedSub 1500 25
            KeyEvent KeyRightbrace e -> modifyLight e $ #kelvin %~ incrementLightField ctrl shift clampedAdd 9000 25
            _ -> pure []
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
            _ -> pure []
  where
    wrap f = runStateT $ setMods >> get >>= f -- TODO this is only separated to stop Fourmolu from indenting
    setMods = case event of
        KeyEvent KeyLeftctrl e -> setMod #ctrl e
        KeyEvent KeyRightctrl e -> setMod #ctrl e
        KeyEvent KeyLeftshift e -> setMod #shift e
        KeyEvent KeyRightshift e -> setMod #shift e
        KeyEvent KeyLeftalt e -> setMod #alt e
        KeyEvent KeyRightalt Pressed -> #modeChangeState ?= Nothing
        _ -> pure ()
      where
        setMod l = \case
            Pressed -> l .= True
            Released -> l .= False
            Repeated -> pure ()
    simpleAct = act . send
    act = pure . pure . ActionEvent mempty
    irOnce = simpleAct .: SendIR IROnce
    irHold = \case
        Pressed -> simpleAct .: SendIR IRStart
        Repeated -> const $ const $ pure []
        Released -> simpleAct .: SendIR IRStop
    hueInterval ctrl shift = 16 * if ctrl then 16 else if shift then 4 else 1
    clampedAdd m a b = b + min (m - b) a -- TODO better implementation? maybe in library? else, this is presumably commutative in last two args (ditto below)
    clampedSub m a b = b - min (b - m) a
    modifyLight e f = act case e of
        Pressed -> setColour False =<< send GetCurrentLight
        Repeated -> setColour True =<< send GetCurrentLight
        Released -> send UnsetLightColourCache
      where
        setColour useCache l = send . SetLightColour True l 0 . f =<< send (GetLightColour useCache l)
    incrementLightField ctrl shift f bound inc = if ctrl then const bound else f bound if shift then inc * 4 else inc
    startSpotifySearch t = #typing ?= (TypingSpotifySearch t, []) >> pure [LogEvent "Waiting for keyboard input"]

feed :: (S.MonadAsync m, MonadLog Text m) => [Text] -> Mode -> Opts -> S.Stream m [Event]
feed keyboardNames initialMode opts =
    scanStream
        ( KeyboardState
            { keyboards = mempty
            , mode = initialMode
            , previousMode = Normal
            , shift = False
            , ctrl = False
            , alt = False
            , modeChangeState = Nothing
            , typing = Nothing
            }
        )
        ( uncurry \d ->
            runStateT
                . either
                    ( either
                        ( \() -> do
                            logMessage $ "Evdev device added: " <> decodeUtf8 (Evdev.devicePath d)
                            #keyboards %= Set.insert d
                            -- TODO unify "always grabbed unless in Idle mode" logic somewhere?
                            mode <- use #mode
                            when (mode /= Idle) $ liftIO $ Evdev.grabDevice d
                            pure []
                        )
                        ( \e -> do
                            logMessage $ "Evdev device removed: " <> showT e
                            #keyboards %= Set.delete d
                            pure []
                        )
                    )
                    (StateT . dispatchKeys opts . Evdev.eventData)
        )
        -- I can't find a reliable heuristic for "basically a keyboard" so we filter by name
        . S.filterM (fmap ((`elem` keyboardNames) . decodeUtf8) . liftIO . Evdev.deviceName . fst)
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
