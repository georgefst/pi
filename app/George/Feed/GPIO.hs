module George.Feed.GPIO (feed, Opts (..)) where

import George.Core
import Util
import Util.GPIO qualified as GPIO

import Control.Concurrent
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Time
import Streamly.Data.Stream.Prelude qualified as S

data Opts = Opts
    { gpioChip :: ByteString
    , buttonDebounce :: Double
    , buttonPin :: Int
    }

feed :: (S.MonadAsync m) => Opts -> S.Stream m [Event]
feed opts = streamWithInit (liftIO newEmptyMVar) \m ->
    (S.catMaybes . S.parList id)
        [ S.fromEffect
            . liftIO
            $ (\() -> Nothing)
                <$> GPIO.mon
                    opts.gpioChip
                    (putMVar m . pure . LogEvent)
                    opts.buttonDebounce
                    opts.buttonPin
                    -- TODO we'd ideally use this is a power button, but for noew we just monitor it
                    -- since there have been issues with electrical interference causing spurious triggers
                    (putMVar m . pure . LogEvent . ("GPIO button pressed: " <>) . showT =<< getCurrentTime)
        , S.repeatM $ Just <$> liftIO (takeMVar m)
        ]
