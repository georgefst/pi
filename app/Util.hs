{-# OPTIONS_GHC -Wno-orphans #-}

module Util where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Freer
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either.Extra
import Data.List.Extra
import Data.Text qualified as T
import Data.Text.Encoding hiding (Some)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Network.Socket
import Options.Generic
import RawFilePath
import Streamly.Data.Stream qualified as S
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

-- | A simple wrapper in lieu of first-class existential types.
data Exists t where
    Exists :: t a -> Exists t

withExists :: (forall a. t a -> b) -> Exists t -> b
withExists f (Exists a) = f a

(.:) :: (c -> c') -> (a -> b -> c) -> a -> b -> c'
(.:) = (.) . (.)

-- TODO there should really be a simpler way to implement this with folds
scanStream :: (Monad f) => s -> (a -> s -> (b, s)) -> S.Stream f a -> S.Stream f b
scanStream s0 f = fmap snd . S.runStateT (pure s0) . S.mapM (state . f) . S.morphInner lift

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

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' = threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds