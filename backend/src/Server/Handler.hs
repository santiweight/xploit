module Server.Handler where

import "servant-snap" Servant.Server
import Snap.Core
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import "servant-snap" Servant
import Poker.Range
import Poker.Base
import Control.Monad.IO.Class
import Control.Monad
import Poker.Parse.Base
import Poker.Filter.Parser.Parser
import Poker.Filter.Eval.AST.Base (forget)
import Poker.Filter.Eval.Base
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Esqueleto
import Database.Persist.Postgresql
import Data.Maybe
import Common.Server.Api
import Server.Base
import Server.Query


addServer :: MonadSnap m => Server Add l m
addServer = addHandsServer :<|> addHandFileServer

addHandFileServer :: MonadSnap m => Server AddHandFile l m
addHandFileServer contentsMay = do
          case contentsMay of
            Just contents ->
              parseString contents & \case
                Right hands -> do
                  liftIO $ forM_ hands insertHand
                  pure ()
                Left _ -> throwError err401
            Nothing -> throwError err400

echoServer :: MonadSnap m => Server Echo l m
echoServer = liftIO $ print @String "echoing" >> pure ()

addHandsServer :: MonadSnap m => Server LoadHandHAPI l m
addHandsServer fpMay = do
          -- liftIO $ print "adding contents:"
          -- liftIO $ print fpMay
          -- case fpMay of
          --   Just contents ->
          --     parseString contents & \case
          --       Right hands -> do
          --         liftIO $ forM_ hands insertHand
          --         pure ()
          --       Left _ -> throwError err401
          --   Nothing -> throwError err400
      liftIO $ print @String "adding hands to server"
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
      -> m (Map String (Range Holding [BetAction]))
    queryHandler (Just queryStr) = do
      hands <- liftIO $ runDb selectAllHands
      -- queryStr <- liftIO $ readFile path
      let query' = forget . parseQuery $ queryStr
      -- let resultRange =
      let results = runQuery query' <$> hands
      -- let noErrorResults = filter isEmpty <$> results
      let firstResults = fromMaybe (Right $ Map.empty)
                       . safeHead
                       . filter isEmpty <$> results
      -- liftIO $ print noErrorResults
      -- liftIO $ print results
      let noErrorResults :: [Map String (Map Holding [BetAction])]
                         = mapMaybe (preview _Right) firstResults
      let noErrorResult = foldr (Map.unionWith unionRanges) (Map.empty) noErrorResults
      -- -- let result :: Either
      -- --            (Either GameErrorBundle EvalErr)
      -- --            (Map String (Range Holding [BetAction]))
      -- --            = fmap (fmap Range) $ foldr accResults (Right Map.empty) firstResults
      -- -- liftIO $ print result
      -- -- liftIO $ print results
      liftIO $ print firstResults
      -- liftIO $ print noErrorResults
      -- liftIO $ print noErrorResult
      -- pure result
      pure $ Range <$> noErrorResult
      -- pure Map.empty

      -- pure resultRange
    queryHandler _ = throwError err400
    isEmpty (Right _) = True
    isEmpty _ = False
    -- isEmpty (Right m) = m /= Map.empty
    -- isEmpty _ = False
    -- accResults
    --   :: (Ord k1, Ord k)
    --   => Either e (Map k1 (Map k [a]))
    --   -> Either e (Map k1 (Map k [a]))
    --   -> Either e (Map k1 (Map k [a]))
    -- accResults l r = liftM2 (Map.unionWith unionRanges) l r
    unionRanges :: Ord k => Map k [a] -> Map k [a] -> Map k [a]
    unionRanges m1 m2 = Map.unionWith (++) m1 m2
    safeHead [] = Nothing
    safeHead (x : _) = Just x

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
pokerServer = (queryServer :<|> addServer :<|> echoServer)

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
