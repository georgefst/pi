{- TODO this is intended to eventually form the core of a library:
George's
Effective (pun!)
Organiser of
Receiving and
Generating
Events
-}

module George.Core where

import Util
import Util.GPIO qualified as GPIO
import Util.Lifx

import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage)
import Control.Monad.State.Strict
import Data.ByteString qualified as B
import Data.Char (isSpace)
import Data.Foldable
import Data.List
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Data.Stream.Infinite qualified as Stream
import Data.Text qualified as T
import Data.Time
import Evdev (KeyEvent (..))
import Evdev.Codes (Key (..))
import GHC.Records (HasField)
import Lifx.Lan (HSBK, MonadLifx)
import Lifx.Lan qualified as Lifx
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString hiding (send)
import Optics
import Optics.State.Operators
import Options.Generic
import Spotify (MonadSpotify (throwClientError))
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
import System.Process.Extra

data AppState = AppState
    { activeLEDs :: Map Int GPIO.Handle
    , bulbs :: Stream.Stream (Lifx.Device, Lifx.LightState)
    , httpConnectionManager :: Manager
    , keySendSocket :: Socket
    , lightColourCache :: Maybe HSBK
    }
    deriving (Generic)

data Event where
    ActionEvent :: (Show a) => (a -> IO ()) -> (CompoundAction a) -> Event
    LogEvent :: Text -> Event
    ErrorEvent :: Error -> Event
runEventStream ::
    (MonadIO m) =>
    (Error -> m ()) ->
    (Text -> m ()) ->
    (forall a. Action a -> ExceptT Error m a) ->
    S.Stream m [Event] ->
    m ()
runEventStream handleError log' run' =
    S.fold
        ( SF.drainMapM \case
            ErrorEvent e -> handleError e
            LogEvent t -> log' t
            ActionEvent f action -> (either handleError pure <=< runExceptT) $ runM do
                r <-
                    action & translate \a -> do
                        lift . log' $ showT a
                        run' a
                sendM . lift . log' $ showT r
                sendM . liftIO $ f r
        )
        . S.concatMap S.fromList
        . S.cons [LogEvent "Starting..."]

data Error where
    Error :: (Show a) => {title :: Text, body :: a} -> Error
    SimpleError :: Text -> Error
deriving instance Show Error

-- TODO what I really want is just to catch all non-async exceptions
-- is there no good way to do this? maybe by catching all then re-throwing asyncs?
-- it does seem to be difficult - https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell
-- TODO on the other hand, should the other exception types used here be made subtypes of `IOException`?
catchActionErrors :: forall m a. (MonadCatch m, MonadError Error m) => m a -> m a
-- catchActionErrors = catchMany @[IOException, HttpException] $ throwError . Error "Error when running action"
catchActionErrors =
    catchMany'
        [ Exists $ Proxy @IOException
        , Exists $ Proxy @HttpException
        , Exists $ Proxy `asProxyTypeOfFunc` throwClientError @IO
        ]
        $ throwError . Error "Error when running action"
  where
    -- TODO this is just a cute/ugly trick to make up for the fact that Spotify library throws an unexported error type
    asProxyTypeOfFunc :: proxy x -> (x -> y) -> proxy x
    asProxyTypeOfFunc = const

type CompoundAction a = Eff '[Action] a
data Action a where
    Exit :: Action ()
    ResetError :: Action ()
    Sleep :: NominalDiffTime -> Action ()
    SetLED :: Int -> Bool -> Action ()
    SetSystemLEDs :: Bool -> Action ()
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
runAction opts@ActionOpts{setLED {- TODO GHC doesn't yet support impredicative fields -}} = (.) catchActionErrors \case
    Exit -> liftIO exitSuccess
    ResetError -> setLED opts.ledErrorPin False
    Sleep t -> liftIO $ threadDelay' t
    SetLED n b -> setLED n b
    SetSystemLEDs b ->
        traverse_
            (\(l, v) -> liftIO $ readProcess "sudo" ["tee", "/sys/class/leds/" <> l <> "/trigger"] (v <> "\n"))
            (if b then [("ACT", "mmc0"), ("PWR", "default-on")] else [("ACT", "none"), ("PWR", "none")])
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
