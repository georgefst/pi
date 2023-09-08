module George.Feed.GPIO (feed, Opts (..)) where

import George.Core
import Util
import Util.GPIO qualified as GPIO

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Time
import Streamly.Data.Stream.Prelude qualified as S

data Opts = Opts
    { gpioChip :: ByteString
    , buttonDebounce :: Double
    , buttonPin :: Int
    }

feed :: Opts -> S.Stream IO [Event]
feed opts = emitterToStream \f ->
    GPIO.mon
        opts.gpioChip
        (f . pure . LogEvent)
        opts.buttonDebounce
        opts.buttonPin
        -- TODO we'd ideally use this is a power button, but for noew we just monitor it
        -- since there have been issues with electrical interference causing spurious triggers
        (f . pure . LogEvent . ("GPIO button pressed: " <>) . showT =<< liftIO getCurrentTime)
