module Server.DB.Query where

import Codec.Serialise (deserialise)
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
    (^.),
  )
import Poker
import Poker.History.Bovada.Model
import Server.Base
  ( EntityField (HandHTableTy),
    HandH (handHSerial),
  )

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
