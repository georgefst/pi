module George.Feed.WebServer (webServer) where

import George.Core

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Freer
import Data.Foldable
import Network.Wai qualified as Wai
import Okapi hiding (Event, error, get, head)
import Okapi qualified hiding (head)
import Options.Generic

-- we only use this for actions which return a response
webServer :: (forall m. (MonadIO m) => Event -> m ()) -> Wai.Application
webServer f =
    makeOkapiApp id $
        asum
            [ withGetRoute "light" $ f' id (send . GetLightName =<< send GetCurrentLight)
            ]
  where
    withGetRoute s x = Okapi.get >> seg s >> x
    f' :: (Show a) => (a -> Text) -> CompoundAction a -> OkapiT IO Result
    f' show' x = do
        m <- liftIO newEmptyMVar
        f $ ActionEvent (putMVar m) x
        okPlainText [] . (<> "\n") . show' =<< liftIO (takeMVar m)
