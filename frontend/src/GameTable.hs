{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module GameTable where

import BasicPrelude (tshow)
import qualified BasicPrelude as P
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Text as T
import Poker
import Poker.Game.Types
import Poker.Query.ActionIx
import PrettyBetAmount
import Reflex.Dom

gameTable ::
  ( PrettyBetAmount b,
    IsBetSize b,
    DomBuilder t m
  ) =>
  GameState b ->
  m ()
gameTable gameState = divClass "table" $ do
  potEl
  boardEl
  players
  where
    potEl = divClass "pot" $ text $ prettyPot (_potSize gameState)
    board = _street gameState
    prettyPot (Pot amt) = prettyBetAmount amt
    getCards :: Board -> [Card]
    getCards b = case b of
      RiverBoard ca bo -> ca : getCards bo
      TurnBoard ca bo -> ca : getCards bo
      FlopBoard (c1, c2, c3) bo -> c1 : c2 : c3 : getCards bo
      PreFlopBoard _ -> []
      InitialTable -> []
    boardEl = divClass "card-place" $
      P.forM_ (getCards board) $ \(Card r s) ->
        divClass ("card figures-" <> T.take 1 (tshow s) <> " values-" <> P.tshow r) $ do
          el "h1" $ text (T.pack $ show r)
          divClass "figures D" $ pure ()
          el "h1" $ text (T.pack $ show r)
    players = divClass "players" $ do
      let activePlayer = gameState ^. toActQueue . to P.head
      P.forM_ (Map.toList $ gameState ^. posToStack) $ \(pos, stack) -> do
        playerEl
          (activePlayer == pos)
          stack
          pos
          (gameState ^. streetInvestments . at pos . non mempty)

playerEl ::
  (PrettyBetAmount b, DomBuilder t m) =>
  -- |
  Bool ->
  -- |
  Stack b ->
  -- |
  Position ->
  -- |
  b ->
  m ()
playerEl active stack pos streetInv =
  divClass ("player player-" <> P.tshow pos <> " playing") $ do
    divClass "bank" $ do
      divClass "bank-value" $ text $ prettyBetAmount streetInv
    divClass ("player-tag " <> if active then "active" else "inactive") $ do
      divClass "name" $ text $ P.tshow pos
      divClass "stack" $ text $ prettyBetAmount $ stack
