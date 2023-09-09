module George.Feed.WebServer (feed) where

import George.Core

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Freer
import Data.Functor
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Okapi hiding (Event)
import Streamly.Data.Stream.Prelude qualified as S
import Util.Streamly.Okapi qualified as Okapi
import Util.Util

-- TODO update to new Okapi as soon as it's released: https://github.com/monadicsystems/okapi/issues/30
feed :: Warp.Port -> S.Stream IO [Event]
feed port =
    S.catMaybes $
        Okapi.stream
            Okapi.Opts
                { warpSettings = Warp.setPort port Warp.defaultSettings
                , routes =
                    [ withGetRoute "tmp" $ f showT $ send ResetError
                    , withGetRoute "light" $ f id $ send . GetLightName =<< send GetCurrentLight
                    , withGetRoute "spotify" do
                        seg "transfer"
                        deviceName <- segParam
                        f showT $ send . flip SpotifyTransfer True =<< send (SpotifyGetDevice deviceName)
                    ]
                }
            <&> \case
                Okapi.Event x -> Just [x]
                Okapi.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f show' a = do
        m <- liftIO newEmptyMVar
        pure
            ( ActionEvent (putMVar m) a
            , okPlainText [] . (<> "\n") . show' =<< liftIO (takeMVar m)
            )
    withGetRoute s x = Okapi.get >> seg s >> x
