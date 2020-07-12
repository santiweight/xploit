{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Backend where

import Frontend
import Common.Api
import Common.Route
import Data.Text (Text)
import Obelisk.Backend
import Obelisk.Route
import "servant-snap" Servant.Server (serveSnap, Server)
import Servant.API ((:>), (:<|>) (..), Capture, Get, JSON)
import Snap.Core (MonadSnap)
import Data.Proxy
import Control.Monad.IO.Class

-- apiServer :: MonadSnap m => m ()
-- apiServer = serveSnap (Proxy @MyAPI) server

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      (BackendRoute_Api     :/ _)  -> serveSnap (Proxy :: Proxy PokerAPI) pokerServer
      (BackendRoute_Missing :/ ()) -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
