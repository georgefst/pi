-- TODO get a proper Haskell GPIO library (hpio?) working with the modern interface
module Util.GPIO (Handle, reset, set, mon) where

import Util

import Control.Monad (void)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Loops (iterateM_)
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Options.Generic (Text)
import RawFilePath (CreatePipe (CreatePipe), proc, processStdout, readProcessWithExitCode, setStdout, startProcess)
import System.IO (hGetLine)

-- TODO this is a weird interface for good reason - I adapted this from some code for Clark, my other RPi
-- and want to make it easy to revert the implementation without changing client code
-- on that device, `gpioset` is not persistent, and I'm not sure why this differs - maybe RPi kernel vs. NixOS?
-- from documentation I've seen, the other, non-persistent, behaviour seems to be what is supposed to happen
data Handle = Handle ByteString [Int]
reset :: (MonadIO m) => Handle -> m ()
reset (Handle gpioChip xs) =
    void . liftIO $
        readProcessWithExitCode . proc "gpioset" $
            gpioChip : map ((<> "=0") . showBS) xs
set :: (MonadIO m) => ByteString -> [Int] -> m Handle
set gpioChip xs = do
    void . liftIO $
        readProcessWithExitCode . proc "gpioset" $
            gpioChip : map ((<> "=1") . showBS) xs
    pure $ Handle gpioChip xs

mon :: ByteString -> (Text -> IO ()) -> Double -> Int -> IO () -> IO ()
mon gpioChip putLine debounce pin x = do
    p <-
        startProcess $
            proc "gpiomon" ["-b", "-f", gpioChip, showBS pin]
                `setStdout` CreatePipe
    getCurrentTime >>= iterateM_ \t0 -> do
        line <- hGetLine $ processStdout p
        t1 <- getCurrentTime
        if diffUTCTime t1 t0 < realToFrac debounce
            then putLine $ "(Ignoring) " <> T.pack line
            else putLine (T.pack line) >> x
        pure t1
