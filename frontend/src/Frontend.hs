{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend where

import BasicPrelude (void)
import qualified BasicPrelude as P
import BetWidgets
import Common.Route
import Common.Server.Api
import Control.Lens
  ( snoc,
  )
import DB.Stats
import Data.Functor ((<&>))
import qualified Data.Text as T
import GHC.Stack
import GameLogic
import GameTable
import qualified Head
import Obelisk.Frontend
  ( Frontend (..),
    ObeliskWidget,
  )
import Obelisk.Route
import Obelisk.Route.Frontend (RoutedT, routeLink, subRoute_)
import Poker
import Poker.Game.Types
import Prettyprinter
import RangeCalc
  ( getCurrentNode,
  )
import RangeDisplay
  ( holdingDisplay,
    rangeDisplayWidget,
  )
import Reflex.Dom.Core
import Review.Widget (review)

frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = Head.head,
      _frontend_body = subRoute_ $ \case
        FrontendRoute_Main -> body
        FrontendRoute_Review -> review
        FrontendRoute_Stats -> void $ prerender (pure ()) statsWidget
    }

body ::
  forall js t m r.
  (ObeliskWidget js t (R FrontendRoute) m) =>
  RoutedT t r m ()
body = prerender_ (pure ()) $ do
  routeLink (FrontendRoute_Review :/ ()) $ text "review"
  routeLink (FrontendRoute_Stats :/ ()) $ text "stats"
  el "div" $ text "include hero? Note: does nothing right now..."
  includeHeroD <- _inputElement_checked <$> checkBox
  rec tableWidget $ currGameState <$> gameTreeD
      -- The fact that betBtns changes with the updated state, means that the bet buttons change
      -- the frame _after_ the game state changes. This causes duplicate requests for the tree, with
      -- the first request being invalid. In order to avoid this, using updated is incorrect, and furthermore,
      -- betBtns needs to be based on a dyn construct, I believe.
      filterBetD <- betBtns initRangeState $ updated (currGameState <$> gameTreeD)
      nodeLockEv <- lockNodeBtn filterBetD
      gameTreeD <-
        foldDyn
          ($)
          (mkRoot initRangeState)
          (leftmost [accNodesFun <$> nodeLockEv, const <$> selectNodeEv])
      selectNodeEv <- switchHold never =<< dyn (treeView filterBetD <$> gameTreeD)
  normD <- fmap (\case True -> NormToBB; False -> NoNorm) . _inputElement_checked <$> checkBox
  filterBetD' <-
    forDynM filterBetD $
      ( \(pos, ba) -> do
          gs <- sample (current (currGameState <$> gameTreeD))
          pure (Stake (unsafeMkAmount 25), pos, ba)
      )
  nodeQueryResponseD <-
    getCurrentNode
      filterBetD'
      -- ((\(pos, ba) -> (_stateStakes gs, pos, ba)) <$> filterBetD)
      (getNodePath <$> gameTreeD)
      includeHeroD
      normD
  prerender_ (pure ()) $ do
    selectShapedHandD <-
      rangeDisplayWidget
        (nodeQueryResponseD <&> shapedHandRange)
    _ <- holdingDisplay selectShapedHandD (nodeQueryResponseD <&> holdingRange)
    matchedHandsDisplay nodeQueryResponseD
    pure ()
  pure ()
  where
    getNodePath = fmap (\(pos, act, _) -> (pos, act)) . restNodes

    matchedHandsDisplay nodeQueryResponseD =
      dyn_ $
        (nodeQueryResponseD <&> handsMatchedFilter) <&> \matchedHands -> do
          P.forM_ matchedHands $ \matchedHand -> el "p" $ do
            -- TODO get the hand's text here :)
            let handLines = fmap T.pack . lines . take 20 $ show matchedHand -- _handText matchedHand
            P.forM_ handLines $ \handLine -> do
              el "div" $ text handLine

    checkBox =
      inputElement
        ( def
            & inputElementConfig_elementConfig
              . elementConfig_initialAttributes
            .~ ("type" =: "checkbox")
        )

    tableWidget gameStateDyn =
      dyn_ $
        gameStateDyn <&> \gameState ->
          divClass "vue-container" $ gameTable gameState

    lockNodeBtn filterBetD = el "div" $ do
      lockNodeEv <- button "Lock Node"
      pure $ tag (current filterBetD) lockNodeEv

    accNodesFun ::
      (Pretty b, IsBet b) =>
      (Position, BetAction b) ->
      GameTree b (BetAction b) ->
      GameTree b (BetAction b)
    accNodesFun (pos, act) gameTree = case gameTree of
      Root init [] -> Root init [(pos, act, doPosAct (pos, act) init)]
      Root init nodes ->
        let (_, _, prevGameSt) = P.last nodes
         in Root init (nodes `snoc` (pos, act, doPosAct (pos, act) prevGameSt))

data GameTree b act = Root
  { rootGameState :: GameState b,
    restNodes :: [(Position, act, GameState b)]
  }
  deriving (Show)

treeView ::
  (DomBuilder t m, Show act, PostBuild t m) =>
  Dynamic t (Position, act) ->
  GameTree b act ->
  m (Event t (GameTree b act))
treeView currFiltActD (Root init lockedNodes) = do
  _ <- (Root init [] <$) <$> el "div" (button "Root")
  nodeEvs <- leftmost <$> nodeBtns [] lockedNodes
  _ <-
    dyn_ $
      currFiltActD
        <&> (\currFiltAct -> el "div" (button $ uncurry prettyNode currFiltAct))
  pure nodeEvs
  where
    nodeBtns _ [] = pure []
    nodeBtns prevNodes [(pos, act, _)] = do
      selectNodeEv <- el "div" . button $ prettyNode pos act
      pure . (: []) $ Root init prevNodes <$ selectNodeEv
    nodeBtns prevNodes (node@(pos, act, _) : ns) = do
      selectNodeEv <- el "div" . button $ prettyNode pos act
      restNodeBtnEvs <- nodeBtns (prevNodes ++ [node]) ns
      pure $ (Root init prevNodes <$ selectNodeEv) : restNodeBtnEvs
    prettyNode pos act = P.tshow pos <> ": " <> P.tshow act

mkRoot :: GameState b -> GameTree b act
mkRoot gameSt = Root gameSt []

currGameState :: HasCallStack => GameTree a b -> GameState a
currGameState (Root initState nodes) = case nodes of
  [] -> initState
  _ -> (\(_, _, gameSt) -> gameSt) $ last nodes
