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
import           Poker
import           PrettyBetAmount
import           Reflex.Dom
import Poker.Query.ActionIx
import Poker.Game.Types
import RangeDisplay (prettyText)

gameTable
  :: ( PrettyBetAmount b
     , IsBetSize b
     , DomBuilder t m)
  => GameState b
  -> m ()
gameTable gameState = divClass "table" $ do
  potEl
  boardEl
  players
 where
  potEl = divClass "pot" $ text $ prettyPot (_potSize gameState)
  prettyPot (Pot amt) = prettyBetAmount amt
  boardEl = divClass "card-place" $ P.forM_ [2 .. 6] $ \num ->
    divClass ("card figures-D values-" <> P.tshow @Integer num) $ do
      el "h1" $ text (T.pack $ show num)
      divClass "figures D" $ pure ()
      el "h1" $ text (T.pack $ show num)
  players = divClass "players" $ do
    let activePlayer = gameState ^. toActQueue . to P.head
    P.forM_ (Map.toList $ gameState ^. posToStack) $ \(pos, stack) -> do
      playerEl (activePlayer == pos)
               stack
               pos
               (gameState ^. streetInvestments . at pos . non mempty)

playerEl
  :: ( PrettyBetAmount b, DomBuilder t m)
  =>Bool -- ^
  -> Stack b -- ^
  -> Position -- ^
  -> b -- ^
  -> m ()
playerEl active stack pos streetInv =
  divClass ("player player-" <> P.tshow pos <> " playing") $ do
    divClass "bank" $ do
      divClass "bank-value" $ text $ prettyBetAmount streetInv
    divClass ("player-tag " <> if active then "active" else "inactive") $ do
      divClass "name" $ text $ P.tshow pos
      divClass "stack" $ text $ prettyBetAmount $ stack
