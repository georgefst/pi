{- HLINT ignore "Redundant section" -}
module George.Feed.Keyboard (feed, Opts (..), Mode (..)) where

import George.Core
import Util

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Bifunctor
import Data.Foldable
import Data.IORef
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
import Util.Streamly qualified as S
import Util.Util
import Prelude hiding (log)

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
    , typing :: Maybe (TypingReason, [Char], IO ())
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

dispatchKeys :: Opts -> Evdev.EventData -> KeyboardState -> IO (S.Stream IO [Event], KeyboardState)
dispatchKeys opts = wrap \case
    (KeyRightalt, e, KeyboardState{modeChangeState, previousMode}) -> case e of
        Pressed -> #modeChangeState ?= Nothing
        _ -> case modeChangeState of
            Nothing -> err $ Error "Unexpected mode switch key event" e
            Just mk -> case e of
                Repeated -> pure ()
                Released -> (either err pure <=< runExceptT) do
                    #modeChangeState .= Nothing
                    new <- case mk of
                        Nothing -> pure previousMode
                        Just k -> case k of
                            KeyEsc -> pure Idle
                            KeySlash -> pure Idle
                            KeyQ -> pure Quiet
                            KeyDot -> pure Normal
                            KeyT -> pure TV
                            KeyComma -> pure Sending
                            _ -> throwError $ Error "Key does not correspond to any mode" k
                    lift $ switchMode new
    (k, _, KeyboardState{modeChangeState = Just _}) ->
        #modeChangeState ?= Just k
    (k, e, KeyboardState{mode = Sending}) -> simpleAct $ SendKey k e
    (KeyLeftctrl, e, _) -> setMod #ctrl e
    (KeyRightctrl, e, _) -> setMod #ctrl e
    (KeyLeftshift, e, _) -> setMod #shift e
    (KeyRightshift, e, _) -> setMod #shift e
    (KeyLeftalt, e, _) -> setMod #alt e
    (k, Pressed, KeyboardState{typing = Just (t, cs, finish), shift}) -> case k of
        KeyEsc -> finishTyping >> log "Discarding keyboard input"
        KeyEnter -> (finishTyping >>) case t of
            TypingSpotifySearch searchType ->
                act $ send . SpotifySearchAndPlay searchType text =<< send (SpotifyGetDevice speakerName)
          where
            text = T.pack $ reverse cs
        KeyBackspace -> #typing % mapped % _2 %= tailSafe
        _ -> case keyToChar shift k of
            Just c -> #typing % mapped % _2 %= (c :)
            Nothing -> log $ "Ignoring non-character keypress" <> mwhen shift " (with shift)" <> ": " <> showT k
      where
        finishTyping = #typing .= Nothing >> liftIO finish
    (k, e, KeyboardState{..}) -> case mode of
        Idle -> pure ()
        Quiet -> case (k, e) of
            (KeySpace, Pressed) -> night False
            _ -> pure ()
        Normal -> case k of
            KeyVolumeup -> irHold e IRHifi "KEY_VOLUMEUP"
            KeyVolumedown -> irHold e IRHifi "KEY_VOLUMEDOWN"
            KeyLeft -> modifyLight $ #hue %~ subtract hueInterval
            KeyRight -> modifyLight $ #hue %~ (+ hueInterval)
            KeyMinus -> modifyLight $ #saturation %~ incrementLightField clampedSub minBound 256
            KeyEqual -> modifyLight $ #saturation %~ incrementLightField clampedAdd maxBound 256
            KeyDown -> modifyLight $ #brightness %~ incrementLightField clampedSub minBound 256
            KeyUp -> modifyLight $ #brightness %~ incrementLightField clampedAdd maxBound 256
            KeyLeftbrace -> modifyLight $ #kelvin %~ incrementLightField clampedSub 1500 25
            KeyRightbrace -> modifyLight $ #kelvin %~ incrementLightField clampedAdd 9000 25
            _ -> case e of
                Pressed -> case k of
                    KeyEsc | ctrl, shift, alt -> simpleAct PowerOff
                    KeyEsc | ctrl, shift -> simpleAct Reboot
                    KeyEsc | ctrl -> simpleAct Exit
                    KeyR | ctrl -> simpleAct ResetError
                    KeyL | ctrl, shift -> startTyping $ TypingSpotifySearch Spotify.AlbumSearch
                    KeyA | ctrl, shift -> startTyping $ TypingSpotifySearch Spotify.ArtistSearch
                    KeyP | ctrl, shift -> startTyping $ TypingSpotifySearch Spotify.PlaylistSearch
                    KeyS | ctrl, shift -> startTyping $ TypingSpotifySearch Spotify.TrackSearch
                    KeyW | ctrl, shift -> startTyping $ TypingSpotifySearch Spotify.ShowSearch
                    KeyE | ctrl, shift -> startTyping $ TypingSpotifySearch Spotify.EpisodeSearch
                    KeyB | ctrl, shift -> startTyping $ TypingSpotifySearch Spotify.AudiobookSearch
                    KeySpace -> night True
                    KeyP -> act do
                        wasOn <- send GetHifiPlugPower
                        if wasOn
                            then do
                                send $ SendIR IRHifi "KEY_POWER"
                                send $ Sleep 3
                                send $ SetHifiPlugPower False
                            else do
                                send $ SetHifiPlugPower True
                                send $ Sleep 1
                                send $ SendIR IRHifi "KEY_POWER"
                                send $ Sleep 1
                                send $ SendIR IRHifi "KEY_TAPE"
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
                    KeyMute -> irOnce IRHifi "KEY_MUTING"
                    KeyPlaypause -> simpleAct $ Mpris "PlayPause"
                    KeyPrevioussong -> simpleAct $ Mpris "Previous"
                    KeyNextsong -> simpleAct $ Mpris "Next"
                    KeyR -> simpleAct LightReScan
                    KeyL -> act do
                        p <- send . GetLightPower =<< send GetCurrentLight
                        getLightOrGroup >>= traverse_ \l -> send . SetLightPower l $ not p
                    KeyDot -> act $ send . flip SpotifyTransfer ctrl =<< send (SpotifyGetDevice speakerName)
                    KeyT -> irOnce IRTV "KEY_POWER"
                    _ -> pure ()
                _ -> pure ()
          where
            hueInterval = 16 * if ctrl then 16 else if shift then 4 else 1
            clampedAdd m a b = b + min (m - b) a -- TODO better implementation? maybe in library? else, this is presumably commutative in last two args (ditto below)
            clampedSub m a b = b - min (b - m) a
            modifyLight f = act case e of
                Pressed -> setColour False
                Repeated -> setColour True
                Released -> send UnsetLightColourCache
              where
                setColour useCache = do
                    c <- send . GetLightColour useCache =<< send GetCurrentLight
                    getLightOrGroup >>= traverse_ \l -> send . SetLightColour True l 0 $ f c
            incrementLightField f bound inc = if ctrl then const bound else f bound if shift then inc * 4 else inc
            getLightOrGroup =
                if alt
                    then send . GetLightsInGroup =<< send GetCurrentLightGroup
                    else pure <$> send GetCurrentLight
        TV -> case k of
            KeySpace | e == Pressed -> act do
                send $ SendIR IRTV "KEY_AUX"
                send $ Sleep t
                send $ SendIR IRTV "KEY_AUX"
                send $ Sleep t
                send $ SendIR IRTV "KEY_OK"
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
        fmap (\(((), s), es) -> (S.parList id es, s)) . runWriterT . runStateT case e0 of
            KeyEvent k e -> get >>= \s -> f (k, e, s)
            _ -> pure ()
    setMod l = \case
        Pressed -> l .= True
        Released -> l .= False
        Repeated -> pure ()
    evs = tell . pure . S.fromPure
    err = evs . pure . ErrorEvent
    log = evs . pure . LogEvent
    switchMode new = do
        keyboards <- use #keyboards
        old <- use #mode
        #mode .= new
        #previousMode .= old
        when (old == Idle) $ traverse_ (liftIO . Evdev.grabDevice) keyboards
        when (new == Idle) $ traverse_ (liftIO . Evdev.ungrabDevice) keyboards
        evs
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
    simpleAct = act . send
    act = evs . pure . ActionEvent mempty
    irOnce = simpleAct .: SendIR
    irHold = \case
        Pressed -> simpleAct .: SendIR
        Repeated -> simpleAct .: SendIR
        Released -> const $ const $ pure ()
    startTyping t = do
        ref <- liftIO $ newIORef True
        #typing ?= (t, [], writeIORef ref False)
        mode <- use #mode
        log "Waiting for keyboard input"
        tell $ pure case opts.modeLED mode of
            Nothing -> S.nil
            Just led ->
                flip S.append (S.fromPure [setLED True])
                    . S.takeWhileM (const $ liftIO $ readIORef ref)
                    . S.concatMap id
                    . S.repeat
                    $ (S.consM pause . S.cons [setLED False] . S.consM pause . S.cons [setLED True] $ S.nil)
              where
                pause = liftIO (threadDelay 300_000) >> pure []
                setLED = ActionEvent mempty . send . SetLED led
    night b = do
        act $
            traverse_ (send . flip SetLightPower (not b))
                =<< send (GetLightsInGroup livingRoomLightGroup)
        switchMode if b then Quiet else Normal
    speakerName = "pi"
    -- TODO search for `label = "Living Room"` instead? actually not easily done
    livingRoomLightGroup = "\x63\xCD\x89\x80\xDF\x31\xC6\xD5\xF6\xAC\xD\xDF\x12\x5B\x55\x7B"

feed :: [Text] -> Mode -> Opts -> S.Stream IO [Event]
feed keyboardNames initialMode opts =
    (((S.parConcat id . fmap snd) .) . S.runStateT . pure)
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
        . uncurry S.cons
        . second
            ( S.mapM
                ( uncurry \d ->
                    either
                        ( either
                            ( \() -> do
                                #keyboards %= Set.insert d
                                -- TODO unify "always grabbed unless in Idle mode" logic somewhere?
                                mode <- use #mode
                                when (mode /= Idle) $ liftIO $ Evdev.grabDevice d
                                pure $ S.fromPure [LogEvent $ "Evdev device added: " <> decodeUtf8 (Evdev.devicePath d)]
                            )
                            ( \e -> do
                                #keyboards %= Set.delete d
                                pure $ S.fromPure [LogEvent $ "Evdev device removed: " <> showT e]
                            )
                        )
                        (StateT . dispatchKeys opts . Evdev.eventData)
                )
                . S.morphInner lift
                -- I can't find a reliable heuristic for "basically a keyboard" so we filter by name
                . S.filterM (fmap ((`elem` keyboardNames) . decodeUtf8) . liftIO . Evdev.deviceName . fst)
                . readEventsMany
            )
        -- TODO I'm a bit worried about what this might do to fusion - generalise `readEventsMany` instead?
        . S.partitionEithers
        . S.mapM
            ( either
                ( \(p, e) ->
                    (pure . Left)
                        [LogEvent $ "Couldn't create evdev device from " <> decodeUtf8 p <> ": " <> showT e]
                )
                (pure . Right)
            )
        . S.append allDevices
        $ newDevices' 1_000_000
