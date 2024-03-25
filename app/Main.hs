{- HLINT ignore "Use <=<" -}
module Main (main) where

import George.Core
import George.Feed.Keyboard qualified as Keyboard
import George.Feed.WebServer qualified as WebServer
import Util
import Util.GPIO.Persistent qualified as GPIO
import Util.Lifx

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.State.Strict
import Data.Bool
import Data.ByteString qualified as B
import Data.List.Extra
import Data.List.NonEmpty (nonEmpty)
import Data.Map qualified as Map
import Data.Maybe
import Data.Stream.Infinite qualified as Stream
import Data.Text.IO qualified as T
import Data.Word
import Lifx.Lan qualified as Lifx
import Network.HTTP.Client
import Network.Socket
import Network.Wai.Handler.Warp qualified as Warp
import Optics
import Options.Generic
import Streamly.Data.Stream.Prelude qualified as S
import System.Exit
import System.IO
import Text.Pretty.Simple
import Util.Util

data Opts = Opts
    { gpioChip :: B.ByteString
    , buttonPin :: Int
    , ledErrorPin :: Int
    , ledIdleModePin :: Int
    , ledSendingModePin :: Int
    , ledNormalModePin :: Int
    , ledTvModePin :: Int
    , lifxTimeout :: Double
    , lifxIgnore :: [Text]
    , lifxPort :: Word16
    , httpPort :: Warp.Port
    , keyboard :: [Text]
    , keySendPort :: PortNumber
    , keySendIps :: [IP]
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier = fieldNameModifier lispCaseModifiers}

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering -- TODO necessary when running as systemd service - why? report upstream
    (opts :: Opts) <- getRecord "Pi"

    let
        setLED :: (MonadState AppState m, MonadIO m, MonadLog Text m) => Int -> Bool -> m ()
        setLED pin =
            bool
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Just h -> GPIO.reset h >> modifying #activeLEDs (Map.delete pin)
                    Nothing -> logMessage $ "LED is already off: " <> showT pin
                )
                ( use #activeLEDs <&> Map.lookup pin >>= \case
                    Nothing -> GPIO.set opts.gpioChip [pin] >>= modifying #activeLEDs . Map.insert pin
                    Just _ -> logMessage $ "LED is already on: " <> showT pin
                )

        handleError :: (MonadIO m, MonadState AppState m, MonadLog Text m) => Error -> m ()
        handleError err = do
            case err of
                Error{title, body} -> do
                    liftIO . T.putStrLn $ title <> ":"
                    pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsInitialIndent = 4} body
                SimpleError t -> liftIO $ T.putStrLn t
            setLED opts.ledErrorPin True

        modeLED = \case
            Keyboard.Idle -> Just opts.ledIdleModePin
            Keyboard.Quiet -> Nothing
            Keyboard.Sending -> Just opts.ledSendingModePin
            Keyboard.Normal -> Just opts.ledNormalModePin
            Keyboard.TV -> Just opts.ledTvModePin
        initialMode = Keyboard.Idle

    initialState <- do
        bulbs <- pure $ error "no bulbs"
        pure
            AppState
                { activeLEDs = mempty
                , lightColourCache = Nothing
                , ..
                }

    flip evalStateT initialState
        . flip runLoggingT (liftIO . T.putStrLn)
        . (>>= either (handleError . Error "LIFX error") pure)
        . Lifx.runLifxT
            (lifxTime opts.lifxTimeout)
            (Just $ fromIntegral opts.lifxPort)
        . runEventStream handleError logMessage (runAction (opts & \Opts{..} -> ActionOpts{..}))
        . S.morphInner liftIO
        . S.append
            -- flash all lights to show we have finished initialising
            ( S.fromList
                . map (pure . ActionEvent mempty . send)
                $ concatMap
                    (\n -> [SetLED n True, Sleep 0.2, SetLED n False])
                    (mapMaybe modeLED enumerate <> [opts.ledErrorPin])
                    <> [Sleep 0.5]
                    <> maybe mempty (pure . flip SetLED True) (modeLED initialMode)
            )
        $ S.parList
            id
            [ Keyboard.feed opts.keyboard initialMode Keyboard.Opts{..}
            , WebServer.feed opts.httpPort
            -- TODO disabled until logging is better
            -- it's easier to see events when monitoring through a separate script
            -- , GPIO.feed (opts & \Opts{..} -> GPIO.Opts{..})
            ]
