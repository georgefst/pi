module George.Feed.WebServer (feed) where

import George.Core

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Freer
import Data.Foldable
import Data.Tuple.Extra
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Okapi hiding (Event, error, get, head)
import Okapi qualified hiding (head)
import Options.Generic
import Streamly.Data.Stream.Prelude qualified as S
import Streamly.Internal.Data.Stream.StreamK qualified as SK

-- we only use this for actions which return a response
server :: (forall m. (MonadIO m) => Event -> m ()) -> Wai.Application
server f =
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

feed :: (S.MonadAsync m) => Warp.Port -> S.Stream m [Event]
feed port = SK.toStream $ SK.unCross do
    m <- liftIO newEmptyMVar
    -- TODO is there a better way to encapsulate getting initial mvar in to a stream than all this faffing with `SK`?
    SK.mkCross . SK.fromStream . S.catMaybes $
        S.parList
            id
            [ S.fromEffect $
                liftIO $
                    (\() -> Nothing)
                        <$> Warp.runSettings
                            ( Warp.setLogger
                                (curry3 $ unless . statusIsSuccessful . snd3 <*> putMVar m . ErrorEvent . Error "HTTP error")
                                . Warp.setPort port
                                $ Warp.defaultSettings
                            )
                            (server $ liftIO . putMVar m)
            , S.repeatM $ Just . pure <$> liftIO (takeMVar m)
            ]
