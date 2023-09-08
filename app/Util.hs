{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
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
pattern Exists' :: () => t a -> Exists' t
pattern Exists' a = Exists a
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

-- TODO can this be implemented more efficiently in a single pass?
streamPartitionEithers :: (Monad m) => S.Stream m (Either a b) -> (S.Stream m a, S.Stream m b)
streamPartitionEithers s = (S.catLefts s, S.catRights s)

-- bool indicates whether shift held
-- TODO make this more complete and general and less anglocentric...
keyToChar :: Bool -> Evdev.Key -> Maybe Char
keyToChar = \case
    False -> \case
        Evdev.KeyMinus -> Just '-'
        Evdev.KeyEqual -> Just '='
        Evdev.KeyLeftbrace -> Just '['
        Evdev.KeyRightbrace -> Just ']'
        Evdev.KeySemicolon -> Just ';'
        Evdev.KeyApostrophe -> Just '\''
        Evdev.KeyGrave -> Just '#'
        Evdev.KeyBackslash -> Just '\\'
        Evdev.KeyComma -> Just ','
        Evdev.KeyDot -> Just '.'
        Evdev.KeySlash -> Just '/'
        Evdev.KeySpace -> Just ' '
        Evdev.Key1 -> Just '1'
        Evdev.Key2 -> Just '2'
        Evdev.Key3 -> Just '3'
        Evdev.Key4 -> Just '4'
        Evdev.Key5 -> Just '5'
        Evdev.Key6 -> Just '6'
        Evdev.Key7 -> Just '7'
        Evdev.Key8 -> Just '8'
        Evdev.Key9 -> Just '9'
        Evdev.Key0 -> Just '0'
        Evdev.KeyA -> Just 'a'
        Evdev.KeyB -> Just 'b'
        Evdev.KeyC -> Just 'c'
        Evdev.KeyD -> Just 'd'
        Evdev.KeyE -> Just 'e'
        Evdev.KeyF -> Just 'f'
        Evdev.KeyG -> Just 'g'
        Evdev.KeyH -> Just 'h'
        Evdev.KeyI -> Just 'i'
        Evdev.KeyJ -> Just 'j'
        Evdev.KeyK -> Just 'k'
        Evdev.KeyL -> Just 'l'
        Evdev.KeyM -> Just 'm'
        Evdev.KeyN -> Just 'n'
        Evdev.KeyO -> Just 'o'
        Evdev.KeyP -> Just 'p'
        Evdev.KeyQ -> Just 'q'
        Evdev.KeyR -> Just 'r'
        Evdev.KeyS -> Just 's'
        Evdev.KeyT -> Just 't'
        Evdev.KeyU -> Just 'u'
        Evdev.KeyV -> Just 'v'
        Evdev.KeyW -> Just 'w'
        Evdev.KeyX -> Just 'x'
        Evdev.KeyY -> Just 'y'
        Evdev.KeyZ -> Just 'z'
        Evdev.KeyTab -> Just '\t'
        _ -> Nothing
    True -> \case
        Evdev.KeyMinus -> Just '_'
        Evdev.KeyEqual -> Just '+'
        Evdev.KeyLeftbrace -> Just '{'
        Evdev.KeyRightbrace -> Just '}'
        Evdev.KeySemicolon -> Just ':'
        Evdev.KeyApostrophe -> Just '@'
        Evdev.KeyGrave -> Just '~'
        Evdev.KeyBackslash -> Just '|'
        Evdev.KeyComma -> Just '<'
        Evdev.KeyDot -> Just '>'
        Evdev.KeySlash -> Just '?'
        Evdev.Key1 -> Just '!'
        Evdev.Key2 -> Just '"'
        Evdev.Key3 -> Just '£'
        Evdev.Key4 -> Just '$'
        Evdev.Key5 -> Just '%'
        Evdev.Key6 -> Just '^'
        Evdev.Key7 -> Just '&'
        Evdev.Key8 -> Just '*'
        Evdev.Key9 -> Just '('
        Evdev.Key0 -> Just ')'
        Evdev.KeyA -> Just 'A'
        Evdev.KeyB -> Just 'B'
        Evdev.KeyC -> Just 'C'
        Evdev.KeyD -> Just 'D'
        Evdev.KeyE -> Just 'E'
        Evdev.KeyF -> Just 'F'
        Evdev.KeyG -> Just 'G'
        Evdev.KeyH -> Just 'H'
        Evdev.KeyI -> Just 'I'
        Evdev.KeyJ -> Just 'J'
        Evdev.KeyK -> Just 'K'
        Evdev.KeyL -> Just 'L'
        Evdev.KeyM -> Just 'M'
        Evdev.KeyN -> Just 'N'
        Evdev.KeyO -> Just 'O'
        Evdev.KeyP -> Just 'P'
        Evdev.KeyQ -> Just 'Q'
        Evdev.KeyR -> Just 'R'
        Evdev.KeyS -> Just 'S'
        Evdev.KeyT -> Just 'T'
        Evdev.KeyU -> Just 'U'
        Evdev.KeyV -> Just 'V'
        Evdev.KeyW -> Just 'W'
        Evdev.KeyX -> Just 'X'
        Evdev.KeyY -> Just 'Y'
        Evdev.KeyZ -> Just 'Z'
        _ -> Nothing
