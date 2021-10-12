{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Frontend where


import           Obelisk.Frontend               ( Frontend(..)
                                                , ObeliskWidget
                                                )
import           Obelisk.Route
import           Reflex.Dom.Core

import qualified BasicPrelude                  as P
import           BetWidgets
import           Common.Route
import           Common.Server.Api
import           Control.Lens                   ( snoc

                                                )
import           Data.Functor                   ( (<&>) )
import qualified Data.Text                     as T
import           GameLogic
import           GameTable
import qualified Head
import           Obelisk.Route.Frontend         ( RoutedT )
import           Poker
import           Poker.Game.Types
import           RangeCalc                      ( getCurrentNode
                                                )
import           RangeDisplay                   ( holdingDisplay
                                                , rangeDisplay
                                                )

frontend :: Frontend (R FrontendRoute)
frontend = Frontend { _frontend_head = Head.head, _frontend_body = body }

body
  :: forall js t m
   . (ObeliskWidget js t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
body = do
  includeHeroD <- _inputElement_checked <$> checkBox
  rec
    tableWidget $ currGameState <$> gameTreeD
    filterBetD <- betBtns initRangeState $ updated (currGameState <$> gameTreeD)
    nodeLockEv <- lockNodeBtn filterBetD
    gameTreeD  <- foldDyn
      ($)
      (mkRoot initRangeState)
      (leftmost [accNodesFun <$> nodeLockEv, const <$> selectNodeEv])
    selectNodeEv <- switchHold never =<< dyn (treeView filterBetD <$> gameTreeD)
  nodeQueryResponseD <- getCurrentNode filterBetD
                                       (getNodePath <$> gameTreeD)
                                       includeHeroD
  selectShapedHandD <- rangeDisplayWidget
    (nodeQueryResponseD <&> shapedHandRange)
  _ <- holdingDisplay selectShapedHandD (nodeQueryResponseD <&> holdingRange)
  matchedHandsDisplay nodeQueryResponseD
 where
  getNodePath = fmap (\(pos, act, _) -> (pos, act)) . restNodes

  matchedHandsDisplay nodeQueryResponseD =
    dyn_ $ (nodeQueryResponseD <&> handsMatchedFilter) <&> \matchedHands -> do
      P.forM_ matchedHands $ \matchedHand -> el "p" $ do
        -- TODO get the hand's text here :)
        let handLines = fmap T.pack . lines $ "matched hand" -- _handText matchedHand
        P.forM_ handLines $ \handLine -> do
          el "div" $ text handLine

  checkBox = inputElement
    (  def
    &  inputElementConfig_elementConfig
    .  elementConfig_initialAttributes
    .~ ("type" =: "checkbox")
    )

  tableWidget gameStateDyn = dyn_ $ gameStateDyn <&> \gameState ->
    divClass "vue-container" $ gameTable gameState

  rangeDisplayWidget currRange = do
    selectShapedHandE <- rangeDisplay currRange
    holdDyn (mkPair Ace) selectShapedHandE

  lockNodeBtn filterBetD = el "div" $ do
    lockNodeEv <- button "Lock Node"
    pure $ tagPromptlyDyn filterBetD lockNodeEv

  accNodesFun :: _
  accNodesFun (pos, act) gameTree = case gameTree of
    Root init [] -> Root init [(pos, act, doPosAct (pos, act) init)]
    Root init nodes ->
      let (_, _, prevGameSt) = P.last nodes
      in  Root init (nodes `snoc` (pos, act, doPosAct (pos, act) prevGameSt))

data GameTree b act = Root
  { rootGameState :: GameState b
  , restNodes     :: [(Position, act, GameState b)]
  }
  deriving Show

treeView
  :: (DomBuilder t m, Show act, PostBuild t m)
  => Dynamic t (Position, act)
  -> GameTree b act
  -> m (Event t (GameTree b act))
treeView currFiltActD (Root init lockedNodes) = do
  _       <- (Root init [] <$) <$> el "div" (button "Root")
  nodeEvs <- leftmost <$> nodeBtns [] lockedNodes
  _       <-
    dyn_
    $   currFiltActD
    <&> (\currFiltAct -> el "div" (button $ uncurry prettyNode currFiltAct))
  pure nodeEvs
 where
  nodeBtns _         []              = pure []
  nodeBtns prevNodes [(pos, act, _)] = do
    selectNodeEv <- el "div" . button $ prettyNode pos act
    pure . (: []) $ Root init prevNodes <$ selectNodeEv
  nodeBtns prevNodes (node@(pos, act, _) : ns) = do
    selectNodeEv   <- el "div" . button $ prettyNode pos act
    restNodeBtnEvs <- nodeBtns (prevNodes ++ [node]) ns
    pure $ (Root init prevNodes <$ selectNodeEv) : restNodeBtnEvs
  prettyNode pos act = P.tshow pos <> ": " <> P.tshow act

mkRoot :: GameState b -> GameTree b act
mkRoot gameSt = Root gameSt []

currGameState :: GameTree a b -> GameState a
currGameState (Root initState nodes) = case nodes of
  [] -> initState
  _  -> (\(_, _, gameSt) -> gameSt) $ last nodes

