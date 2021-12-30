{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Common.Route
  ( BackendRoute (..),
    FrontendRoute,
    fullRouteEncoder,
  )
import Common.Server.Api (PokerAPI)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))
import Obelisk.Backend (Backend (..))
import Obelisk.Route
import "servant-snap" Servant.Server (serveSnap)
import Server.Base
import Server.Handler (pokerServer)
import Prelude hiding (read)

-- apiServer :: MonadSnap m => m ()
-- apiServer = serveSnap (Proxy @MyAPI) server

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> serve $ \case
        (BackendRoute_Api :/ _) -> do
          liftIO $ migrateDB
          serveSnap (Proxy :: Proxy PokerAPI) pokerServer
        (BackendRoute_Missing :/ ()) -> return (),
      _backend_routeEncoder = fullRouteEncoder
    }