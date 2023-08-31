{- | Functions for working with streams of input events.
Unless stated otherwise, these functions will throw exceptions if the underlying C calls fail.
-}
module Evdev.Stream (
    allDevices,
    allEvents,
    makeDevices,
    newDevices,
    newDevices',
    readEvents,
    readEventsMany,
) where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Bool
import Data.Either.Extra
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Evdev
import RawFilePath.Directory
import Streamly.Data.Stream qualified as S
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Internal.Data.Stream qualified as SI
import System.FilePath.ByteString
import System.INotify qualified as INotify
import System.IO
import System.IO.Error

-- TODO provide a 'group' operation on streams, representing packets as sets

-- | Read all events from a device.
readEvents :: (MonadIO m) => Device -> S.Stream m Event
readEvents = unfoldM . liftIO . printIOError' . nextEvent

{- | Concurrently read events from multiple devices.
If a read fails on one, the exception is printed to stderr and the stream continues to read from the others.
-}
readEventsMany :: (S.MonadAsync m) => S.Stream m Device -> S.Stream m (Device, Event)
readEventsMany = S.parConcatMap id \d -> (d,) <$> readEvents d

-- | Create devices for all paths in the stream.
makeDevices :: (MonadIO m) => S.Stream m RawFilePath -> S.Stream m Device
makeDevices = S.mapM $ liftIO . newDevice

-- {- | All events on all valid devices (in /\/dev\/input/).
-- Prints any exceptions.

-- > allEvents == readEventsMany allDevices
-- -}
allEvents :: (S.MonadAsync m) => S.Stream m (Device, Event)
allEvents = readEventsMany allDevices

-- TODO call this 'oldDevices' or 'existingDevices', and have 'allDevices' include 'newDevices'?

{- | All valid existing devices (in /\/dev\/input/).
If a device can't be initialised for an individual path, then the exception is printed,
and the function continues to try to initialise the others.
-}
allDevices :: (MonadIO m) => S.Stream m Device
allDevices =
    let paths =
            S.filterM (liftIO . doesFileExist) $
                fmap (evdevDir </>) $
                    SI.unCross $
                        (SI.mkCross . S.fromList)
                            =<< SI.mkCross (S.fromEffect (reverse <$> liftIO (listDirectory evdevDir)))
     in S.mapMaybeM (liftIO . printIOError' . newDevice) paths

-- TODO perhaps streamly-fsnotify ought to use RawFilePath?
-- TODO fix this - we don't always seem to get notified of permission changes -
-- indeed when we don't, we actually find that 'stat' and 'ls -l' show different permissions to:
-- 'fmap (flip showOct "" . fileMode) . getFileStatus'

-- {- | All new devices created (in /\/dev\/input/).
-- Watches for new file paths (using \inotify\), and those corresponding to valid devices are added to the stream.
-- -}
newDevices :: (MonadIO m) => S.Stream m Device
newDevices =
    let
        -- 'watching' keeps track of the set of paths which have been added, but don't yet have the right permissions
        watch :: (MonadIO m) => Set RawFilePath -> INotify.Event -> m (Maybe Device, Set RawFilePath)
        watch watching =
            liftIO . \case
                INotify.Created{isDirectory = False, filePath = p} ->
                    tryNewDevice p <&> \case
                        Right d ->
                            -- success - return new device
                            (Just d, watching)
                        Left e ->
                            -- fail - if it's only a permission error then watch for changes on device
                            (Nothing, applyWhen (isPermissionError e) (Set.insert p) watching)
                INotify.Modified{isDirectory = False, maybeFilePath = Just p} ->
                    if p `elem` watching
                        then
                            tryNewDevice p <&> \case
                                Right d ->
                                    -- success - no longer watch for changes
                                    (Just d, Set.delete p watching)
                                Left _ ->
                                    -- fail - continue to watch
                                    (Nothing, watching)
                        else -- this isn't an event we care about
                            return (Nothing, watching)
                INotify.Deleted{isDirectory = False, filePath = p} ->
                    -- device is gone - no longer watch for changes
                    return (Nothing, Set.delete p watching)
                _ -> return (Nothing, watching)
        tryNewDevice = printIOError . newDevice
     in
        scanMaybe watch Set.empty $ watchDirectory evdevDir

-- TODO just fix 'newDevices'

{- | This is a workaround for bugginess in 'newDevices' when it comes to waiting for permissions on a new device
- it just waits the number of microseconds given before trying to read from the device.
-}
newDevices' :: (MonadIO m) => Int -> S.Stream m Device
newDevices' delay = flip S.mapMaybeM (watchDirectory evdevDir) \case
    INotify.Created False p -> do
        liftIO $ threadDelay delay
        eitherToMaybe <$> liftIO (printIOError $ newDevice $ evdevDir </> p)
    _ -> return Nothing

-- {- Util -}

-- specialized form of S.scanlM'
-- for each a, f updates s, and possibly produces a new b, to add to the output stream
-- I really can't think of a good name for this...
-- TODO perhaps some way to use State monad instead?
scanMaybe :: (Monad m) => (s -> a -> m (Maybe b, s)) -> s -> S.Stream m a -> S.Stream m b
scanMaybe f e = S.mapMaybe fst . SI.scanlM' (f . snd) (pure (Nothing, e))

-- specialised form of S.unfoldrM
-- this should perhaps be in streamly (it's in monad-loops)
-- TODO this is rather ugly - can it be done in terms of the Unfold type?
unfoldM :: (Monad m) => m (Maybe a) -> S.Stream m a
unfoldM x = S.unfoldrM (const $ fmap (,()) <$> x) ()

-- TODO get rid - this isn't a great approach for a library
-- like tryIOError, but also prints the error to stderr
printIOError :: IO a -> IO (Either IOError a)
printIOError f =
    (Right <$> f) `catchIOError` \err -> do
        hPrint stderr err
        return $ Left err

-- variant of printIOError which doesn't care what the exception was
printIOError' :: IO a -> IO (Maybe a)
printIOError' = fmap eitherToMaybe . printIOError

-- apply the function iff the guard passes
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

watchDirectory :: (MonadIO m) => RawFilePath -> S.Stream m INotify.Event
watchDirectory path = SI.unCross do
    -- TODO clean up with `removeWatch _wd` on exit somehow
    (m, _wd) <- SI.mkCross $ S.fromEffect $ liftIO do
        m <- newEmptyMVar
        inotify <- INotify.initINotify
        wd <- INotify.addWatch inotify [INotify.Create] path (putMVar m)
        pure (m, wd)
    SI.mkCross $ S.repeatM $ liftIO $ takeMVar m
