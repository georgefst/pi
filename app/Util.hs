{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Freer
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either.Extra
import Data.Function
import Data.Kind
import Data.List.Extra
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding hiding (Some)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Evdev qualified
import Evdev.Codes qualified as Evdev
import Network.Socket
import Options.Generic
import RawFilePath
import Spotify.Types.Misc qualified as Spotify
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Data.StreamK qualified as SK
import Streamly.Internal.Data.Stream.StreamK qualified as SK
import System.Exit

showT :: (Show a) => a -> Text
showT = T.pack . show
showBS :: (Show a) => a -> ByteString
showBS = encodeUtf8 . showT

-- TODO return partial stdout/stderr in timeout case

{- | Essentially `\t -> timeout t . readProcessWithExitCode`, except that it actually works since it uses `SIGTERM`,
whereas `timeout` uses an async exception, and thus isn't good at terminating foreign code.
Time is in microseconds, as with `threadDelay` and `timeout`.
-}
readProcessWithExitCodeTimeout :: Int -> ProcessConf stdin stdout stderr -> IO (Maybe (ExitCode, ByteString, ByteString))
readProcessWithExitCodeTimeout t conf = do
    p <-
        startProcess $
            conf
                `setStdin` NoStream
                `setStdout` CreatePipe
                `setStderr` CreatePipe
    eitherToMaybe @() <$> ((threadDelay t >> terminateProcess p) `race` waitForProcess p)
        >>= traverse \exitCode -> (exitCode,,) <$> B.hGetContents (processStdout p) <*> B.hGetContents (processStderr p)

subsumeFront :: Eff (eff : eff : effs) ~> Eff (eff : effs)
subsumeFront = subsume

-- TODO there must be libraries for this sort of thing
data Exists c t where -- A simple wrapper in lieu of first-class existential types.
    Exists :: (c a) => t a -> Exists c t
withExists :: (forall a. (c a) => t a -> b) -> Exists c t -> b
withExists f (Exists a) = f a
class NullConstraint a
instance NullConstraint a
type Exists' = Exists NullConstraint
class ToProxyList (c :: Type -> Constraint) (ts :: [Type]) where
    toProxyList :: [Exists c Proxy]
instance ToProxyList c '[] where
    toProxyList = []
instance (c t, ToProxyList c ts) => ToProxyList c (t : ts) where
    toProxyList = Exists @c (Proxy @t) : toProxyList @c @ts

-- this is a nicer "modern Haskell" interface than I've seen elsewhere for catching multiple exception types
-- we keep the second version around because it gives slightly more flexibility in obscure cases
catchMany ::
    forall (ts :: [Type]) m a.
    (MonadCatch m, ToProxyList Exception ts) =>
    (forall e. (Exception e) => e -> m a) ->
    m a ->
    m a
catchMany = catchMany' $ toProxyList @Exception @ts
catchMany' ::
    forall m a.
    (MonadCatch m) =>
    [Exists Exception Proxy] ->
    (forall e. (Exception e) => e -> m a) ->
    m a ->
    m a
catchMany' ps h = flip catches . fmap (withExists \(_ :: Proxy e) -> Handler @_ @_ @e h) $ ps

(.:) :: (c -> c') -> (a -> b -> c) -> a -> b -> c'
(.:) = (.) . (.)

-- TODO there should really be a simpler way to implement this with folds
scanStream :: (Monad f) => s -> (a -> s -> f (b, s)) -> S.Stream f a -> S.Stream f b
scanStream s0 f = fmap snd . S.runStateT (pure s0) . S.mapM (StateT . f) . S.morphInner lift

newtype IP = IP {unIP :: HostAddress}
    deriving stock (Generic)
    deriving anyclass (ParseRecord, ParseField, ParseFields)
instance Show IP where
    show (IP x) = intercalate "." $ map show [a, b, c, d]
      where
        (a, b, c, d) = hostAddressToTuple x
instance Read IP where
    readsPrec _ s = case map read $ splitOn "." s of
        [a, b, c, d] -> pure $ (,"") $ IP $ tupleToHostAddress (a, b, c, d)
        _ -> []

deriving anyclass instance ParseField PortNumber
deriving anyclass instance ParseFields PortNumber
instance ParseRecord PortNumber where
    parseRecord = fmap getOnly parseRecord

deriving anyclass instance ParseField NominalDiffTime
deriving anyclass instance ParseFields NominalDiffTime
instance ParseRecord NominalDiffTime where
    parseRecord = fmap getOnly parseRecord

-- this seems to be the minimum boilerplate needed to just inherit the instance for `Text`
instance ParseField Spotify.DeviceID where
    parseField a b c d = Spotify.DeviceID <$> parseField a b c d
    readField = Spotify.DeviceID <$> readField
instance ParseFields Spotify.DeviceID
instance ParseRecord Spotify.DeviceID where
    parseRecord = fmap getOnly parseRecord

instance Eq Evdev.Device where
    (==) = (==) `on` Evdev.devicePath
instance Ord Evdev.Device where
    compare = compare `on` Evdev.devicePath

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' = threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds

mwhen :: (Monoid p) => Bool -> p -> p
mwhen b x = if b then x else mempty

tailSafe :: [a] -> [a]
tailSafe = \case
    [] -> []
    _ : xs -> xs

-- TODO is there a better way to implement this than all this faffing with `SK`?
streamWithInit :: (Monad m) => m t -> (t -> S.Stream m a) -> S.Stream m a
streamWithInit init_ stream = SK.toStream $ SK.unCross do
    m <- SK.mkCross $ SK.fromStream $ S.fromEffect init_
    SK.mkCross . SK.fromStream $ stream m

-- TODO is there a better way to implement this? seems like a common pattern?
emitterToStream :: (S.MonadAsync m) => ((a -> m ()) -> m ()) -> S.Stream m a
emitterToStream f = streamWithInit (liftIO newEmptyMVar) \m ->
    (S.catMaybes . S.parList id)
        [ S.fromEffect $ (\() -> Nothing) <$> f (liftIO . putMVar m)
        , S.repeatM $ Just <$> liftIO (takeMVar m)
        ]

-- bool indicates whether shift held
-- TODO make this more complete and general and less anglocentric...
keyToChar :: Bool -> Evdev.Key -> Maybe Char
keyToChar = curry \case
    (False, Evdev.KeyMinus) -> Just '-'
    (False, Evdev.KeyEqual) -> Just '='
    (False, Evdev.KeyLeftbrace) -> Just '['
    (False, Evdev.KeyRightbrace) -> Just ']'
    (False, Evdev.KeySemicolon) -> Just ';'
    (False, Evdev.KeyApostrophe) -> Just '\''
    (False, Evdev.KeyGrave) -> Just '#'
    (False, Evdev.KeyBackslash) -> Just '\\'
    (False, Evdev.KeyComma) -> Just ','
    (False, Evdev.KeyDot) -> Just '.'
    (False, Evdev.KeySlash) -> Just '/'
    (False, Evdev.KeySpace) -> Just ' '
    (False, Evdev.Key1) -> Just '1'
    (False, Evdev.Key2) -> Just '2'
    (False, Evdev.Key3) -> Just '3'
    (False, Evdev.Key4) -> Just '4'
    (False, Evdev.Key5) -> Just '5'
    (False, Evdev.Key6) -> Just '6'
    (False, Evdev.Key7) -> Just '7'
    (False, Evdev.Key8) -> Just '8'
    (False, Evdev.Key9) -> Just '9'
    (False, Evdev.Key0) -> Just '0'
    (False, Evdev.KeyA) -> Just 'a'
    (False, Evdev.KeyB) -> Just 'b'
    (False, Evdev.KeyC) -> Just 'c'
    (False, Evdev.KeyD) -> Just 'd'
    (False, Evdev.KeyE) -> Just 'e'
    (False, Evdev.KeyF) -> Just 'f'
    (False, Evdev.KeyG) -> Just 'g'
    (False, Evdev.KeyH) -> Just 'h'
    (False, Evdev.KeyI) -> Just 'i'
    (False, Evdev.KeyJ) -> Just 'j'
    (False, Evdev.KeyK) -> Just 'k'
    (False, Evdev.KeyL) -> Just 'l'
    (False, Evdev.KeyM) -> Just 'm'
    (False, Evdev.KeyN) -> Just 'n'
    (False, Evdev.KeyO) -> Just 'o'
    (False, Evdev.KeyP) -> Just 'p'
    (False, Evdev.KeyQ) -> Just 'q'
    (False, Evdev.KeyR) -> Just 'r'
    (False, Evdev.KeyS) -> Just 's'
    (False, Evdev.KeyT) -> Just 't'
    (False, Evdev.KeyU) -> Just 'u'
    (False, Evdev.KeyV) -> Just 'v'
    (False, Evdev.KeyW) -> Just 'w'
    (False, Evdev.KeyX) -> Just 'x'
    (False, Evdev.KeyY) -> Just 'y'
    (False, Evdev.KeyZ) -> Just 'z'
    (False, Evdev.KeyTab) -> Just '\t'
    (True, Evdev.KeyMinus) -> Just '_'
    (True, Evdev.KeyEqual) -> Just '+'
    (True, Evdev.KeyLeftbrace) -> Just '{'
    (True, Evdev.KeyRightbrace) -> Just '}'
    (True, Evdev.KeySemicolon) -> Just ':'
    (True, Evdev.KeyApostrophe) -> Just '@'
    (True, Evdev.KeyGrave) -> Just '~'
    (True, Evdev.KeyBackslash) -> Just '|'
    (True, Evdev.KeyComma) -> Just '<'
    (True, Evdev.KeyDot) -> Just '>'
    (True, Evdev.KeySlash) -> Just '?'
    (True, Evdev.Key1) -> Just '!'
    (True, Evdev.Key2) -> Just '"'
    (True, Evdev.Key3) -> Just '£'
    (True, Evdev.Key4) -> Just '$'
    (True, Evdev.Key5) -> Just '%'
    (True, Evdev.Key6) -> Just '^'
    (True, Evdev.Key7) -> Just '&'
    (True, Evdev.Key8) -> Just '*'
    (True, Evdev.Key9) -> Just '('
    (True, Evdev.Key0) -> Just ')'
    (True, Evdev.KeyA) -> Just 'A'
    (True, Evdev.KeyB) -> Just 'B'
    (True, Evdev.KeyC) -> Just 'C'
    (True, Evdev.KeyD) -> Just 'D'
    (True, Evdev.KeyE) -> Just 'E'
    (True, Evdev.KeyF) -> Just 'F'
    (True, Evdev.KeyG) -> Just 'G'
    (True, Evdev.KeyH) -> Just 'H'
    (True, Evdev.KeyI) -> Just 'I'
    (True, Evdev.KeyJ) -> Just 'J'
    (True, Evdev.KeyK) -> Just 'K'
    (True, Evdev.KeyL) -> Just 'L'
    (True, Evdev.KeyM) -> Just 'M'
    (True, Evdev.KeyN) -> Just 'N'
    (True, Evdev.KeyO) -> Just 'O'
    (True, Evdev.KeyP) -> Just 'P'
    (True, Evdev.KeyQ) -> Just 'Q'
    (True, Evdev.KeyR) -> Just 'R'
    (True, Evdev.KeyS) -> Just 'S'
    (True, Evdev.KeyT) -> Just 'T'
    (True, Evdev.KeyU) -> Just 'U'
    (True, Evdev.KeyV) -> Just 'V'
    (True, Evdev.KeyW) -> Just 'W'
    (True, Evdev.KeyX) -> Just 'X'
    (True, Evdev.KeyY) -> Just 'Y'
    (True, Evdev.KeyZ) -> Just 'Z'
    _ -> Nothing
