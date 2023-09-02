module George.Feed.WebServer (feed) where

import George.Core
import Util

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
feed port = S.morphInner liftIO $ emitterToStream \f ->
    Warp.runSettings
        ( Warp.setLogger
            (curry3 $ unless . statusIsSuccessful . snd3 <*> f . pure . ErrorEvent . Error "HTTP error")
            . Warp.setPort port
            $ Warp.defaultSettings
        )
        (server $ liftIO . f . pure)
