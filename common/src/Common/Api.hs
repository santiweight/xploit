{-# Language DataKinds #-}
{-# Language PackageImports #-}
{-# Language TypeOperators #-}
{-# Language OverloadedStrings #-}

module Common.Api where

import Data.Text (Text)
import "servant-snap" Servant.Server
import Snap.Core
import "servant-snap" Servant
import Poker.Range
import Poker.Base
import Database.Esqueleto
import Database.Persist.Postgresql
import Control.Monad.IO.Class
import Control.Monad
import Poker.Parse.Base
import Poker.Filter.Parser.Parser
import Poker.Filter.Eval.AST.Base (forget)
import Poker.Filter.Eval.Base
import DB.Base
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import DB.Query

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

-- type Api = "api" :> (Add :<|> Sub :<|> Echo)
type Echo = "echo" :> Get '[JSON] Text


-- type Add =
--   "add" :> Capture "x" Integer :> Capture "y" Integer :> Get '[JSON] Integer

-- type Sub =
--   "sub" :> Capture "x" Integer :> Capture "y" Integer :> Get '[JSON] Integer

type QueryAPI = "run"
              -- :> QueryParam "query" String
              :> QueryParam "path" FilePath
              -- :> ReqBody '[JSON] Filter
              :> Get '[JSON] (Range Holding [PlayerActionValue])


type LoadHandHAPI = "add"
                  :> QueryParam "path" FilePath
                  :> Get '[JSON] ()

type PokerAPI = "api" :> (QueryAPI :<|> LoadHandHAPI :<|> Echo)
-- server :: MonadSnap m => Server Api l m
-- server = add :<|> sub :<|> echo
--   where
--     add x y = return (x + y)
--     sub x y = return (x - y)
--     echo = return "echo"


-- apiServer :: MonadSnap m => m ()
-- apiServer = serveSnap api server
echoServer :: MonadSnap m => Server Echo l m
echoServer = pure "echo"

addHandsServer :: MonadSnap m => Server LoadHandHAPI l m
addHandsServer fpMay=
      case fpMay of
        Just fp -> do
          hands <- liftIO $ parseInPath fp
          liftIO $ forM_ hands insertHand
          pure ()
        Nothing -> throwError err400

queryServer :: MonadSnap m => Server QueryAPI l m
queryServer = queryHandler
  where
    -- queryHandler :: Maybe FilePath -> Handler
    --                    (Range ShapedHand (Range Holding [PlayerActionValue]))
                      --  (Range ShapedHand x/100)
    queryHandler
      :: MonadSnap m
      => Maybe String
      -> m (Range Holding [PlayerActionValue])
    queryHandler (Just queryStr) = do
      hands <- liftIO $ runDb selectAllHands
      -- queryStr <- liftIO $ readFile path
      let query' = forget . parseQuery $ queryStr
      let resultRange = -- holdingRangeToShapedRange
                      runQueryOnHands hands query'
      -- let resultFreqRange = shapedHoldingRangeToShapedFreqRange
      --                     . shapedRangeToFreqRange (inIndex (FoldIx))
      --                     $ resultRange
      -- liftIO $ print resultFreqRange
      pure resultRange
    queryHandler _ = throwError err400

runDb :: SqlPersistM a -> IO a
runDb = runResourceT
      . runNoLoggingT
      . withPostgresqlConn connString
      . runSqlConn

-- maybeToHandler :: ServerError -> Maybe a -> Handler a
-- maybeToHandler e = maybe (throwError e) pure

-- server :: MonadSnap m => Server PokerAPI l m
-- server = (queryServer :<|> addHandsServer :<|> echoServer)

pokerServer :: MonadSnap m => Server PokerAPI l m
pokerServer = (queryServer :<|> addHandsServer :<|> echoServer)

-- server :: MonadSnap m => Server Api l m
-- server = add :<|> sub :<|> echo
--   where
--     add x y = return (x + y)
--     sub x y = return (x - y)
--     echo = return "echo"

-- apiServer :: MonadSnap m => m ()
-- apiServer = serveSnap api server

-- type Api = "api" :> (Add :<|> Sub :<|> Echo)

-- type Add =
--   "add" :> Capture "x" Integer :> Capture "y" Integer :> Get '[JSON] Integer

-- type Sub =
--   "sub" :> Capture "x" Integer :> Capture "y" Integer :> Get '[JSON] Integer

-- type Echo = "echo" :> Get '[JSON] Text

-- api :: Proxy Api
-- api = Proxy

-- pokerServer :: MonadSnap m => m ()
-- pokerServer = serveSnap proxy pokerServer
--   where
--     proxy :: Proxy PokerAPI
--     proxy = Proxy
