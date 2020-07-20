module Server.Query where

import Poker.Base
import Database.Esqueleto
import Codec.Serialise (deserialise)
import Data.ByteString.Lazy (fromStrict)
import Server.Base

selectAllHands :: SqlPersistM [Hand]
selectAllHands = do
  hands <- select $
            from $ \hand -> do
              return hand
  return . fmap (toHand . entityVal) $ hands

whereTableTy :: SqlExpr (Entity HandH)
                  -> GameType -> SqlExpr (Value Bool)
whereTableTy hand tableTy = hand ^. HandHTableTy ==. val tableTy

selectHandsWith :: [SqlExpr (Entity HandH) -> SqlExpr (Value Bool)] -> SqlPersistM [Hand]
selectHandsWith preds = do
  hands <- select $ do
    from $ \hand -> do
      mapM_ where_ $ preds <*> pure hand
      return hand
  return . fmap (toHand . entityVal) $ hands

toHand :: HandH -> Hand
toHand = deserialise . fromStrict . handHSerial