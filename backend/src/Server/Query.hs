module Server.Query where

import Poker
import Poker.History.Types
import Database.Esqueleto
import Codec.Serialise (deserialise)
import Data.ByteString.Lazy (fromStrict)
import Server.Base
import Poker.History.Bovada.Model

selectAllHands :: SqlPersistM [History (Amount "USD")]
selectAllHands = do
  hands <- select $
            from $ \hand -> do
              return hand
  return . fmap (toHand . entityVal) $ hands

whereTableTy :: SqlExpr (Entity HandH)
                  -> GameType -> SqlExpr (Value Bool)
whereTableTy hand tableTy = hand ^. HandHTableTy ==. val tableTy

selectHandsWith :: [SqlExpr (Entity HandH) -> SqlExpr (Value Bool)] -> SqlPersistM [History (Amount "USD")]
selectHandsWith preds = do
  hands <- select $ do
    from $ \hand -> do
      mapM_ where_ $ preds <*> pure hand
      return hand
  return . fmap (toHand . entityVal) $ hands

toHand :: HandH -> History (Amount "USD")
toHand = deserialise . fromStrict . handHSerial