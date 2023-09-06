{- HLINT ignore "Redundant section" -}
module George.Feed.Keyboard (feed, Opts (..), Mode (..)) where

import George.Core
import Util

import Control.Monad
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage)
import Control.Monad.State.Strict
import Control.Monad.Writer
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
dispatchKeys opts = wrap \case
    (KeyRightalt, e, KeyboardState{modeChangeState, keyboards, mode, previousMode}) -> case e of
        Pressed -> #modeChangeState ?= Nothing
        _ -> case modeChangeState of
            Nothing -> tell [ErrorEvent $ Error "Unexpected mode switch key event" e]
            Just mk -> case e of
                Repeated -> pure ()
                Released -> (either (tell . pure . ErrorEvent) pure <=< runExceptT) do
                    #modeChangeState .= Nothing
                    let old = mode
                    new <- case mk of
                        Nothing -> pure previousMode
                        Just k -> case k of
                            KeyEsc -> pure Idle
                            KeyQ -> pure Quiet
                            KeyDot -> pure Normal
                            KeyT -> pure TV
                            KeyComma -> pure Sending
                            _ -> throwError $ Error "Key does not correspond to any mode" k
                    #mode .= new
                    #previousMode .= old
                    when (old == Idle) $ traverse_ (liftIO . Evdev.grabDevice) keyboards
                    when (new == Idle) $ traverse_ (liftIO . Evdev.ungrabDevice) keyboards
                    tell
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
    (k, _, KeyboardState{modeChangeState = Just _}) ->
        #modeChangeState ?= Just k
    (k, e, KeyboardState{mode = Sending}) -> simpleAct $ SendKey k e
    (KeyLeftctrl, e, _) -> setMod #ctrl e
    (KeyRightctrl, e, _) -> setMod #ctrl e
    (KeyLeftshift, e, _) -> setMod #shift e
    (KeyRightshift, e, _) -> setMod #shift e
    (KeyLeftalt, e, _) -> setMod #alt e
    (k, Pressed, KeyboardState{typing = Just (t, cs), shift}) -> case k of
        KeyEsc -> #typing .= Nothing >> tell [LogEvent "Discarding keyboard input"]
        KeyEnter -> (#typing .= Nothing >>) case t of
            TypingSpotifySearch searchType ->
                act $ send . SpotifySearchAndPlay searchType text =<< send (SpotifyGetDevice speakerName)
          where
            text = T.pack $ reverse cs
        KeyBackspace -> #typing ?= (t, tailSafe cs)
        _ -> case keyToChar shift k of
            Just c -> #typing ?= (t, c : cs)
            Nothing ->
                tell [LogEvent $ "Ignoring non-character keypress" <> mwhen shift " (with shift)" <> ": " <> showT k]
    (k, e, KeyboardState{..}) -> case mode of
        Idle -> pure ()
        Quiet -> pure ()
        Normal -> case k of
            KeyVolumeup -> irHold e IRHifi "KEY_VOLUMEUP"
            KeyVolumedown -> irHold e IRHifi "KEY_VOLUMEDOWN"
            KeyLeft -> modifyLight e $ #hue %~ subtract (hueInterval ctrl shift)
            KeyRight -> modifyLight e $ #hue %~ (+ hueInterval ctrl shift)
            KeyMinus -> modifyLight e $ #saturation %~ incrementLightField ctrl shift clampedSub minBound 256
            KeyEqual -> modifyLight e $ #saturation %~ incrementLightField ctrl shift clampedAdd maxBound 256
            KeyDown -> modifyLight e $ #brightness %~ incrementLightField ctrl shift clampedSub minBound 256
            KeyUp -> modifyLight e $ #brightness %~ incrementLightField ctrl shift clampedAdd maxBound 256
            KeyLeftbrace -> modifyLight e $ #kelvin %~ incrementLightField ctrl shift clampedSub 1500 25
            KeyRightbrace -> modifyLight e $ #kelvin %~ incrementLightField ctrl shift clampedAdd 9000 25
            _ -> case e of
                Pressed -> case k of
                    KeyEsc | ctrl -> simpleAct Exit
                    KeyR | ctrl -> simpleAct ResetError
                    KeyL | ctrl, shift -> startSpotifySearch Spotify.AlbumSearch
                    KeyA | ctrl, shift -> startSpotifySearch Spotify.ArtistSearch
                    KeyP | ctrl, shift -> startSpotifySearch Spotify.PlaylistSearch
                    KeyS | ctrl, shift -> startSpotifySearch Spotify.TrackSearch
                    KeyW | ctrl, shift -> startSpotifySearch Spotify.ShowSearch
                    KeyE | ctrl, shift -> startSpotifySearch Spotify.EpisodeSearch
                    KeyB | ctrl, shift -> startSpotifySearch Spotify.AudiobookSearch
                    KeyP ->
                        if ctrl
                            then simpleAct ToggleHifiPlug
                            else act do
                                send $ SendIR IROnce IRHifi "KEY_POWER"
                                send $ Sleep 1
                                send $ SendIR IROnce IRHifi "KEY_TAPE"
                    KeyS -> act do
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
                    KeyMute -> irOnce IRHifi "muting"
                    KeyPlaypause -> simpleAct $ Mpris "PlayPause"
                    KeyPrevioussong -> simpleAct $ Mpris "Previous"
                    KeyNextsong -> simpleAct $ Mpris "Next"
                    KeyR -> simpleAct LightReScan
                    KeyL -> act do
                        l <- send GetCurrentLight
                        p <- send $ GetLightPower l
                        send $ SetLightPower l $ not p
                    KeyT -> act $ send . flip SpotifyTransfer ctrl =<< send (SpotifyGetDevice speakerName)
                    _ -> pure ()
                _ -> pure ()
        TV -> case k of
            KeySpace | e == Pressed -> act do
                send $ SendIR IROnce IRTV "KEY_AUX"
                send $ Sleep t
                send $ SendIR IROnce IRTV "KEY_AUX"
                send $ Sleep t
                send $ SendIR IROnce IRTV "KEY_OK"
              where
                t = if ctrl then 1 else 0.35
            KeyP -> irHold e IRTV "KEY_POWER"
            Key1 -> irHold e (if ctrl then IRSwitcher else IRTV) "KEY_1"
            Key2 -> irHold e (if ctrl then IRSwitcher else IRTV) "KEY_2"
            Key3 -> irHold e (if ctrl then IRSwitcher else IRTV) "KEY_3"
            Key4 -> irHold e IRTV "KEY_4"
            Key5 -> irHold e IRTV "KEY_5"
            Key6 -> irHold e IRTV "KEY_6"
            Key7 -> irHold e IRTV "KEY_7"
            Key8 -> irHold e IRTV "KEY_8"
            Key9 -> irHold e IRTV "KEY_9"
            Key0 -> irHold e IRTV "KEY_0"
            KeyVolumeup -> irHold e IRTV "KEY_VOLUMEUP"
            KeyVolumedown -> irHold e IRTV "KEY_VOLUMEDOWN"
            KeyMute -> irHold e IRTV "KEY_MUTE"
            KeyComma -> irHold e IRTV "KEY_CHANNELDOWN"
            KeyDot -> irHold e IRTV "KEY_CHANNELUP"
            KeyA -> irHold e IRTV "KEY_AUX"
            KeyS -> irHold e IRTV "KEY_SETUP"
            KeyR -> irHold e IRTV "KEY_RED"
            KeyT -> irHold e IRTV "KEY_SUBTITLE"
            KeyG -> irHold e IRTV "KEY_G"
            KeyQ -> irHold e IRTV "KEY_MENU"
            KeyUp -> irHold e IRTV "KEY_UP"
            KeyDown -> irHold e IRTV "KEY_DOWN"
            KeyLeft -> irHold e IRTV "KEY_LEFT"
            KeyRight -> irHold e IRTV "KEY_RIGHT"
            KeyEnter -> irHold e IRTV "KEY_OK"
            KeyBackspace -> irHold e IRTV "KEY_BACK"
            KeyI -> irHold e IRTV "KEY_INFO"
            KeyEsc -> irHold e IRTV "KEY_EXIT"
            _ -> pure ()
  where
    wrap f e0 =
        -- this was originally separated to stop Fourmolu from indenting - it's now used to build a sort of DSL
        fmap (\(((), s), es) -> (es, s)) . runWriterT . runStateT case e0 of
            KeyEvent k e -> get >>= \s -> f (k, e, s)
            _ -> pure ()
    setMod l = \case
        Pressed -> l .= True
        Released -> l .= False
        Repeated -> pure ()
    simpleAct = act . send
    act = tell . pure . ActionEvent mempty
    irOnce = simpleAct .: SendIR IROnce
    irHold = \case
        Pressed -> simpleAct .: SendIR IRStart
        Repeated -> const $ const $ pure ()
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
    startSpotifySearch t = #typing ?= (TypingSpotifySearch t, []) >> tell [LogEvent "Waiting for keyboard input"]
    speakerName = "pi"

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
