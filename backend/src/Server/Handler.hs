{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Server.Handler where

import Common.Server.Api
import Control.Lens as L
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Bifunctor (Bifunctor (second))
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import Database.Esqueleto hiding ((<&>))
import Database.Persist.Postgresql
import Database.Redis
import qualified Database.Redis as Redis
import GHC.TypeLits
import Money
import Poker
import qualified Poker.Game.Types
import Poker.History.Bovada.Model
import Poker.History.Bovada.Parser
import Poker.History.Types
import Poker.Query.ActionIx (IxRange (ExactlyRn))
import "servant-snap" Servant
import Server.DB.Schema
import Server.DB.Query (selectAllHands, insertHand, insertReview)
import Server.RunQuery (getReviewRanges, getNode)
import Snap.Core
import System.Directory
import System.Directory.Recursive
import qualified System.FilePath.Find as FP
import Text.Megaparsec

addHandFileServer :: MonadSnap m => Server AddHandFile l m
addHandFileServer contentsMay = undefined -- do
-- liftIO $ print @String "adding hand contents to server"
-- case contentsMay of
--   Just contents -> parseString contents & \case
--     Right hands -> do
--       liftIO $ forM_ hands insertHand
--       pure ()
--     Left _ -> throwError err401
--   Nothing -> throwError err400

echoServer :: MonadSnap m => Server Echo l m
echoServer = liftIO $ print @String "echoing" >> pure ()

loadHandServer :: MonadSnap m => Server LoadHandHAPI l m
-- loadHandServer = (addFilesServer :<|> addDirServer) :<|> addHandFileServer
loadHandServer = addFilesServer

-- addFilesServer :: MonadSnap m => Server _ l m

addFilesServer :: (MonadIO m, MonadSnap m) => Maybe FilePath -> m ()
addFilesServer (Just fp) = do
  liftIO $ print $ "Adding files: " <> show fp
  let handsIO = parseInPath fp
  hands <- liftIO $ handsIO
  liftIO $ forM_ hands insertHand
  pure ()
addFilesServer Nothing = throwError $ err400 {errBody = "No file paths provided"}

addDirServer :: MonadSnap m => Maybe FilePath -> m ()
addDirServer Nothing =
  throwError $ err400 {errBody = "No directory path provided"}
addDirServer (Just dirPath) = do
  liftIO $ print @String "adding dir to server"
  fps <- liftIO findTxtFiles
  let handsIO = fmap concat . sequence $ parseInPath <$> fps
  hands :: [History (Amount "USD")] <- liftIO handsIO
  liftIO $ forM_ hands insertHand
  liftIO $ print $ "Added " <> show (length hands) <> " to db"
  pure ()
  where
    findTxtFiles = FP.find (FP.fileName FP./~? ".?*") FP.always dirPath -- (FP.fileName FP.~~? "*.txt") dir

normaliseReq :: (Amount b -> Amount c) -> NodeQueryRequest b -> NodeQueryRequest c
normaliseReq f =
  \case
    (NodeQueryRequest x0 stake b po ba nor) ->
      NodeQueryRequest
        { nodePath = (fmap . fmap . fmap . fmap) f $ x0,
          nodeStake = fmap f stake,
          includeHero = b,
          nodeExpectedPos = po,
          nodeFilter = (fmap . fmap) f ba,
          nodeNormalisation = nor
        }

queryServer :: MonadSnap m => Server QueryAPI l m
queryServer = queryHandler
  where
    queryHandler :: MonadSnap m => SomeNodeQueryRequest -> m NodeQueryResponse
    queryHandler (SomeNodeQueryRequest USD req) =
      let stake = nodeStake req
       in case nodeNormalisation req of
            NormToBB ->
              let f = (unBigBlind . normalizeAmount stake)
               in go
                    (normaliseReq f req)
                    (\hist@(_handStakes -> stake) -> Just (f <$> hist))
            NoNorm ->
              go
                req
                Just

    go ::
      forall m b.
      ( ToJSON (Amount b),
        KnownSymbol b,
        GoodScale (CurrencyScale b),
        MonadSnap m
      ) =>
      NodeQueryRequest b ->
      (History (Amount "USD") -> Maybe (History (Amount b))) ->
      m NodeQueryResponse
    go request@NodeQueryRequest {..} normFunc = do
      conn <- liftIO $ checkedConnect defaultConnectInfo
      cachedRes <-
        liftIO $
          runRedis conn $
            Redis.get (BL.toStrict $ encode request)
      case cachedRes of
        Left re -> error $ show re
        Right m_bs -> case m_bs of
          Nothing -> do
            -- liftIO migrateDB
            liftIO $ print $ "running query" ++ show nodePath
            hands <- liftIO $ runDb selectAllHands
            liftIO $ print $ "Running query against " <> show (length hands) <> " hands"
            let res = getNode (mapMaybe normFunc hands) request
            liftIO $ runRedis conn $ Redis.set (BL.toStrict $ encode request) (BL.toStrict $ encode res)
            -- liftIO $ print $ res
            pure res
          -- FIXME partial
          Just bs -> pure . fromJust $ (Data.Aeson.decode $ BL.fromStrict bs)

runDb :: SqlPersistM a -> IO a
runDb =
  runResourceT . runNoLoggingT . withPostgresqlConn connString . runSqlConn

-- maybeToHandler :: ServerError -> Maybe a -> Handler a
-- maybeToHandler e = maybe (throwError e) pure

-- server :: MonadSnap m => Server PokerAPI l m
-- server = (queryServer :<|> addFilesServer :<|> echoServer)

handReviewServer :: Server HandReview l Snap
handReviewServer = submitHandler :<|> rangesHandler
  where
    submitHandler reviewHist = do
      reviewHistId <- liftIO $ insertReview reviewHist

      liftIO $ print reviewHist
      liftIO $ print reviewHistId

    rangesHandler :: [Poker.Game.Types.Action (Amount "USD")] -> Snap (Map Int (Range Hand Double, Range ShapedHand Double))
    rangesHandler path = do
      liftIO $ print path
      liftIO $ print "running ranges handler"
      hands <- liftIO $ runDb selectAllHands
      liftIO $ print $ "Running query against " <> show (length hands) <> " hands"
      let foo :: [(Position, BetAction (Amount "USD"))] = go path
      let res = getReviewRanges (second (fmap ExactlyRn) <$> foo) hands
      pure res

go :: [Poker.Game.Types.Action (Amount "USD")] -> [(Position, BetAction (Amount "USD"))]
go bar = case bar of
  [] -> []
  x0 : x1 -> case x0 of
    (ac) -> case ac of
      Poker.Game.Types.MkPlayerAction pa -> case pa of
        Poker.Game.Types.PlayerAction po ba -> (po, ba) : go x1
      Poker.Game.Types.MkDealerAction da -> []
      Poker.Game.Types.MkPostAction pa -> go x1

pokerServer :: Server PokerAPI l Snap
pokerServer = queryServer :<|> loadHandServer :<|> echoServer :<|> handReviewServer :<|> statsServer

statsServer :: Snap (Map String String)
statsServer = do
  allHands <- liftIO $ runDb selectAllHands
  let handsByStakes = foldr go Map.empty allHands
  pure $ Map.fromList [("numHands", show $ length allHands), ("handsByStakes", show handsByStakes)]
  where
    go :: History (Amount "USD") -> Map (Stake (Amount "USD")) Int -> Map (Stake (Amount "USD")) Int
    go (_handStakes -> stakes) m = m & L.at stakes . non 0 L.%~ (+ 1)

parseInPath :: FilePath -> IO [History (Amount "USD")]
parseInPath fp = do
  guard =<< doesPathExist fp
  isDir <- doesDirectoryExist fp
  if isDir
    then do
      allFiles <- getFilesRecursive fp
      allFileContents <- sequence $ T.readFile <$> allFiles
      let hands =
            allFileContents <&> \fileContents -> parse pHands fp fileContents
      pure $
        concat $
          Data.Maybe.mapMaybe rightToMaybe $
            (fmap . fmap . fmap . fmap) unsafeToUsdHand hands
    else error "not a directory"

rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x

-- toUsd :: SomeBetSize -> Amount "USD"
-- toUsd (SomeBetSize USD ra) = unsafeMkAmount . fst . discreteFromDense Floor $ dense' ra
-- toUsd _                    = error "Unexpected non-USD hand"

parseFile ::
  FilePath -> IO (Either (ParseErrorBundle Text Void) [History SomeBetSize])
parseFile f = do
  file <- T.readFile f
  return . parseString $ file

parseString ::
  Text -> Either (ParseErrorBundle Text Void) [History SomeBetSize]
parseString = parse pHands []

-- | Recursively get all files in the given directory.
--
-- @since 0.2.3.0
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive fp = getDirRecursive fp >>= filterM doesFileExist