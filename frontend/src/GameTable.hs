{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module GameTable where

import qualified BasicPrelude                  as P
import           Common.Route
import           Control.Lens
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Obelisk.Frontend
import           Obelisk.Route
import           Poker.Base
import           Poker.Game.Bovada
import           PrettyBetAmount
import           Reflex.Dom

gameTable
  :: ( PrettyBetAmount b
     , ObeliskWidget js t (R FrontendRoute) m
     , IsBetSize b
     , Show b
     )
  => GameState b
  -> m ()
gameTable gameState = divClass "table" $ do
  potEl
  boardEl
  players
 where
  potEl = divClass "pot" $ text $ prettyPot (_potSize gameState)
  prettyPot (PotSize amt) = prettyBetAmount amt
  boardEl = divClass "card-place" $ P.forM_ [2 .. 6] $ \num ->
    divClass ("card figures-D values-" <> P.tshow @Integer num) $ do
      el "h1" $ text (T.pack $ show num)
      divClass "figures D" $ pure ()
      el "h1" $ text (T.pack $ show num)
  players = divClass "players" $ do
    let activePlayer = gameState ^. toActQueue . to P.head
    P.forM_ (Map.assocs $ gameState ^. seatMap) $ \(pos, seat) -> do
      let Just player = gameState ^. playerMap . at seat
      playerEl (activePlayer == pos)
               player
               pos
               (gameState ^. streetInvestments . at pos . non 0)

playerEl
  :: (Show b, PrettyBetAmount b, ObeliskWidget js t (R FrontendRoute) m)
  => Bool -- ^
  -> Player b -- ^
  -> Position -- ^
  -> b -- ^
  -> m ()
playerEl active player pos streetInv =
  divClass ("player player-" <> P.tshow pos <> " playing") $ do
    divClass "bank" $ do
      divClass "bank-value" $ text $ prettyBetAmount streetInv
    divClass ("player-tag " <> if active then "active" else "inactive") $ do
      divClass "name" $ text $ P.tshow pos
      divClass "stack" $ text $ P.tshow $ _stack player
