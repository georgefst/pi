module George.Feed.WebServer (feed) where

import George.Core

import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Data.Functor
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai.Handler.Warp qualified as Warp
import Okapi.App
import Okapi.Response
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
                , routes = \act ->
                    [ lit "tmp" . simpleGet $ f showT act $ send ResetError
                    , lit "light" . simpleGet $ f id act $ send . GetLightName =<< send GetCurrentLight
                    , lit "spotify" . lit "transfer" . param . simpleGet $
                        f showT act . (send . flip SpotifyTransfer True <=< send . SpotifyGetDevice)
                    ]
                }
            <&> \case
                Okapi.Event x -> Just [x]
                Okapi.WarpLog r s i ->
                    guard (not $ statusIsSuccessful s) $> [ErrorEvent (Error "HTTP error" (r, s, i))]
  where
    f show' (act :: Event -> IO ()) a ok _req = do
        m <- newEmptyMVar
        act $ ActionEvent (putMVar m) a
        ok noHeaders . (<> "\n") . show' <$> takeMVar m
    simpleGet = responder @200 @'[] @Text @Text . method GET id
