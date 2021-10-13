{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}

module Backend where

import Prelude hiding (read)
import Common.Server.Api ( PokerAPI )
import Server.Handler ( pokerServer )
import Common.Route
    ( fullRouteEncoder, BackendRoute(..), FrontendRoute )
import Obelisk.Backend ( Backend(..) )
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import "servant-snap" Servant.Server (serveSnap)
import Data.Proxy ( Proxy(..) )
import Data.Text (Text)
import Server.Base


-- apiServer :: MonadSnap m => m ()
-- apiServer = serveSnap (Proxy @MyAPI) server

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      (BackendRoute_Api     :/ _)  -> do
        liftIO $ migrateDB
        serveSnap (Proxy :: Proxy PokerAPI) pokerServer
      (BackendRoute_Missing :/ ()) -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }