{-# LANGUAGE RecordWildCards #-}
module Server.DB.Query where

import Codec.Serialise (deserialise, serialise)
import Data.ByteString.Lazy (fromStrict)
import Database.Esqueleto
  ( Entity (entityVal),
    SqlExpr,
    SqlPersistM,
    Value,
    from,
    select,
    val,
    where_,
    (==.),
    (^.), insertBy, insert
  )
import Poker
import Poker.History.Bovada.Model
import Server.DB.Schema
import Data.Time
import qualified Data.ByteString.Lazy as BL
import Common.Server.Api

selectAllHands :: SqlPersistM [History (Amount "USD")]
selectAllHands = do
  hands <- select $
    from $ \hand -> do
      pure hand
  pure . fmap (toHand . entityVal) $ hands

whereTableTy :: SqlExpr (Entity HandH) -> GameType -> SqlExpr (Value Bool)
whereTableTy hand tableTy = hand ^. HandHTableTy ==. val tableTy

selectHandsWith ::
  [SqlExpr (Entity HandH) -> SqlExpr (Value Bool)] ->
  SqlPersistM [History (Amount "USD")]
selectHandsWith preds = do
  hands <- select $ do
    from $ \hand -> do
      mapM_ where_ $ preds <*> pure hand
      pure hand
  pure . fmap (toHand . entityVal) $ hands

toHand :: HandH -> History (Amount "USD")
toHand = deserialise . fromStrict . handHSerial

insertHand :: History (Amount "USD") -> IO ()
insertHand hand = do
  tz <- getCurrentTimeZone
  (() <$) . runAction connString . insertBy . toHandH tz $ hand
  where
    toHandH :: TimeZone -> History (Amount "USD") -> HandH
    toHandH tz h@History {..} =
      HandH
        { handHHandId = gameId header,
          handHHandHistoryText = _handText,
          handHTableTy = gameTy header,
          handHTime = localTimeToUTC tz $ time header,
          handHSerial = BL.toStrict $ serialise h
        }

insertReview :: ReviewHistory -> IO ReviewHistoryPId
insertReview hist = do
  runAction connString . insert . toReviewHistoryP $ hist
  where
    toReviewHistoryP :: ReviewHistory -> ReviewHistoryP
    toReviewHistoryP = ReviewHistoryP . BL.toStrict . serialise
