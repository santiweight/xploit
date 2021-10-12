module Server.Handler where

import           Common.Server.Api
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Either.Combinators
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import           Data.Void
import           Database.Esqueleto      hiding ( (<&>) )
import           Database.Persist.Postgresql
import           Money
import           Poker
import           Poker.Game.Types
import           Poker.History.Bovada.Model
import           Poker.History.Bovada.Parser
import           Poker.History.Types
import           Poker.Query.ActionIx
import           Poker.Query.Eval.Base          ( runIxBetsAsQuery )
import           Poker.Range
import           Servant
import           Servant.Server
import           Server.Base
import           Server.Query
import           Snap.Core
import           System.Directory
import           System.Directory.Recursive
import           Text.Megaparsec

addServer :: MonadSnap m => Server Add l m
addServer = addHandsServer :<|> addHandFileServer

addHandFileServer :: MonadSnap m => Server AddHandFile l m
addHandFileServer contentsMay = do
  pure ()
          -- case contentsMay of
          --   Just contents ->
          --     parseString contents & \case
          --       Right hands -> do
          --         liftIO $ forM_ hands insertHand
          --         pure ()
          --       Left _ -> throwError err401
          --   Nothing -> throwError err400

echoServer :: MonadSnap m => Server Echo l m
echoServer = liftIO $ print @String "echoing" >> pure ()

addHandsServer :: MonadSnap m => Server LoadHandHAPI l m
addHandsServer fpMay = do
  liftIO $ print @String "adding hands to server"
  case fpMay of
    Just fp -> do
      liftIO $ print fp
      hands <- liftIO $ parseInPath fp
      -- TODO use all hands
      liftIO . print . length . take 10 $ hands
      liftIO $ forM_ hands insertHand
      pure ()
    Nothing -> throwError err400

parseInPath :: FilePath -> IO [History (Amount "USD")]
parseInPath fp = do
  guard =<< doesPathExist fp
  isDir <- doesDirectoryExist fp
  if isDir
    then do
      allFiles        <- getFilesRecursive fp
      allFileContents <- sequence $ T.readFile <$> allFiles
      let
        hands =
          allFileContents <&> \fileContents -> (parse pHands fp $ fileContents)
      pure
        $ concat
        $ Data.Maybe.mapMaybe rightToMaybe
        $ (fmap . fmap . fmap . fmap) toUsd hands
    else error "not a directory"

toUsd :: SomeBetSize -> Amount "USD"
toUsd (SomeBetSize USD ra) = unsafeMkAmount . fst . discreteFromDense Floor $ dense' ra
toUsd _                    = error "Unexpected non-USD hand"

parseFile
  :: FilePath -> IO (Either (ParseErrorBundle Text Void) [History SomeBetSize])
parseFile f = do
  file <- T.readFile f
  return . parseString $ file

parseString
  :: Text -> Either (ParseErrorBundle Text Void) [History SomeBetSize]
parseString = parse pHands []


-- | Recursively get all files in the given directory.
--
-- @since 0.2.3.0
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive fp = getDirRecursive fp >>= filterM doesFileExist

  -- handFileContents <- readFile' fp
  -- let handTexts :: [MatchArray] = matchAll
  --       (makeRegex ("\r\n\r\n\r\n" :: String) :: Regex)
  --       handFileContents
  -- let numHandsExpected = length handTexts
  -- let hs               = either (error . errorBundlePretty) id res
  -- assertEqual "expect all hands parsed" (length hs) (numHandsExpected + 1)

queryServer :: MonadSnap m => Server QueryAPI l m
queryServer = queryHandler . nodePath
 where
  queryHandler
    :: MonadSnap m
    => [(Position, BetAction (IxRange (Amount "USD")))]
    -> m NodeQueryResponse
  queryHandler ixBets = do
    -- let hands = allHands
    hands <- liftIO $ runDb selectAllHands
    liftIO . print $ "running: " <> show ixBets
    liftIO . print $ "using " <> show (length hands) <> " hands"
    let handResultss = flip runIxBetsAsQuery ixBets <$> hands
    -- let handResults = handResultss & mapMaybe safeHead
    let handResults =
          fromMaybe (Right Map.empty) . safeHead . filter isEmpty <$> handResultss
    let noErrorResults =
          mapMaybe (preview _Right) handResults
    let noErrorResult =
          foldr (Map.unionWith unionRanges) Map.empty noErrorResults
    pure $ NodeQueryResponse undefined undefined $ Range <$> noErrorResult
    -- liftIO $ print "results"
    -- -- liftIO $ print firstResults
    -- -- liftIO $ print noErrorResults
    -- liftIO $ print noErrorResult
    -- -- pure result
    -- pure $ Range noErrorResult
  queryHandler _ = throwError err400

  isEmpty (Right _) = True
  isEmpty _         = False
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
  safeHead []      = Nothing
  safeHead (x : _) = Just x

runDb :: SqlPersistM a -> IO a
runDb =
  runResourceT . runNoLoggingT . withPostgresqlConn connString . runSqlConn

-- maybeToHandler :: ServerError -> Maybe a -> Handler a
-- maybeToHandler e = maybe (throwError e) pure

-- server :: MonadSnap m => Server PokerAPI l m
-- server = (queryServer :<|> addHandsServer :<|> echoServer)

pokerServer :: MonadSnap m => Server PokerAPI l m
pokerServer = (queryServer :<|> addServer :<|> echoServer)
