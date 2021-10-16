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

import qualified BasicPrelude as P
import BetWidgets
import Common.Route
import Common.Server.Api
import Common.Util (tshow)
import Control.Arrow ((>>>))
import Control.Lens
  ( snoc, (^.)
  )
import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.Text (Text)
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
import Poker.Game.Bovada (preflopState)
import Poker.Game.Emulate
import Poker.Game.Normalise (Normalise (normalise))
import Poker.Game.Types
import Poker.Game.Utils (runGame)
import Poker.History.Bovada.Model
import Poker.History.Bovada.Parser (pHand, pHands)
import Poker.History.Types (unsafeToUsdHand)
import Prettyprinter
import RangeCalc
  ( getCurrentNode,
  )
import RangeDisplay
  ( holdingDisplay,
    rangeDisplay,
  )
import Reflex.Dom.Core
import Text.Megaparsec (parse)
import JSDOM.Types (liftJSM, Element (unElement), toElement)
import JSDOM (nextAnimationFrame)
import Language.Javascript.JSaddle (jsf, js0, jsgf, js1, JSVal, val, eval, JSM)

frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = Head.head,
      _frontend_body = subRoute_ $ \r -> case r of
        FrontendRoute_Main -> body
        FrontendRoute_Review -> review
    }

defaultHand :: Text
defaultHand =
  "\
  \Bovada Hand #3754009075  Zone Poker ID#1365 HOLDEMZonePoker No Limit - 2019-03-31 05:42:59\n\
  \Seat 1: UTG ($7.76 in chips)\n\
  \Seat 2: UTG+1 ($4.05 in chips)\n\
  \Seat 3: UTG+2 ($6.40 in chips)\n\
  \Seat 4: Dealer ($4.08 in chips)\n\
  \Seat 5: Small Blind ($13.57 in chips)\n\
  \Seat 6: Big Blind [ME] ($5 in chips)\n\
  \Dealer : Set dealer [4]\n\
  \Small Blind : Small Blind $0.02\n\
  \Big Blind  [ME] : Big blind $0.05\n\
  \*** HOLE CARDS ***\n\
  \UTG : Card dealt to a spot [8c Ts]\n\
  \UTG+1 : Card dealt to a spot [7d Tc]\n\
  \UTG+2 : Card dealt to a spot [9s 9c]\n\
  \Dealer : Card dealt to a spot [Jd 4s]\n\
  \Small Blind : Card dealt to a spot [Kd 5d]\n\
  \Big Blind  [ME] : Card dealt to a spot [2d Kc]\n\
  \UTG+1 : Leave(Auto)\n\
  \Dealer : Leave(Auto)\n\
  \UTG : Folds\n\
  \UTG : Leave(Auto)\n\
  \UTG+1 : Folds\n\
  \Small Blind : Leave(Auto)\n\
  \UTG+2 : Raises $0.15 to $0.15\n\
  \Dealer : Folds\n\
  \Small Blind : Folds\n\
  \Big Blind  [ME] : Folds (timeout)\n\
  \Big Blind  [ME] : Seat sit out\n\
  \Big Blind  [ME] : Leave(Auto)\n\
  \UTG+2 : Return uncalled portion of bet $0.10\n\
  \UTG+2 : Does not show [9s 9c] (High Card)\n\
  \UTG+2 : Hand result $0.12\n\
  \UTG+2 : Leave(Auto)\n\
  \Big Blind  [ME] : Enter(Auto)\n\
  \Enter(Auto)\n\
  \Enter(Auto)\n\
  \Table deposit $0.26\n\
  \Enter(Auto)\n\
  \UTG : Enter(Auto)\n\
  \Enter(Auto)\n\
  \*** SUMMARY ***"

review ::
  forall js t m r.
  (ObeliskWidget js t (R FrontendRoute) m) =>
  RoutedT t r m ()
review = mdo
  el "div" $ routeLink (FrontendRoute_Main :/ ()) $ text "home"
  -- let config = def & textAreaElementConfig_initialValue .~ defaultHand & initialAttributes .~ ("rows" =: "20" <> "cols" =: "90")
  let config = def & textAreaElementConfig_initialValue .~ defaultHand & initialAttributes .~ ("rows" =: "20" <> "cols" =: "90")
  inputEl <- el "div" $ textAreaElement config
  let inputTxtDyn = _textAreaElement_value inputEl
  prerender_ (text "rendering") $ do
    el "div" $
      dyn_ $
        inputTxtDyn
          <&> ( parse pHand ""
                  >>> ( \case
                          Left peb -> text $ tshow peb
                          Right his -> do
                            let usdHist = fmap unsafeToUsdHand his
                            let (preflopHand, nonPostActs) = preflopState usdHist
                            let gss = P.scanl' (\gs act -> either (error . show) id $ runGame (emulateAction act) gs) preflopHand nonPostActs
                            (e, unzip -> (actEvs, comments)) <- el' "div" $ do
                              forM (zip [1 ..] nonPostActs) $ \(ix, act) -> do
                                (el_, _) <- el' "h3" $ text $ tshow act
                                commentArea <- el "div" $ textAreaElement def
                                pure $ (ix <$ domEvent Click el_, _textAreaElement_value commentArea)
                            liftJSM $ nextAnimationFrame (\_ -> liftJSM $ do
                              _ <- jsgf ("jQuery" :: Text) [toElement . _element_raw $ e] >>= (^. js1 ("accordion" :: Text) (eval ("({ active: false , collapsible: true })" :: Text)))
                              pure ())
                            el "div" $ dynText $ T.concat <$> sequence comments
                            --  let actsWithGss = zip nonPostActs gss
                            rec ixDyn <- foldDyn ($) 0 (leftmost $ minusOneEv : plusOneEv : (fmap const <$> actEvs))
                                ((subtract 1 <$) -> minusOneEv) <- switchHold never =<< dyn (ixDyn <&> \ix -> if ix == 0 then never <$ text "at ix 0" else button $ "-1")
                                (((+ 1) <$) -> plusOneEv) <- switchHold never =<< dyn (ixDyn <&> \ix -> if ix == length nonPostActs then never <$ text ("at ix " <> tshow ix) else button "+1")
                                let gsDyn = ixDyn <&> \ix -> gss !! ix
                                el "div" $ dyn_ $ gsDyn <&> gameTable
                            pure ()
                      )
              )

    -- liftJSM $ nextAnimationFrame $ \_ -> do
    --   _ <- (unElement . toElement $ _element_raw e) ^. jsf ("annotation" :: Text) ([] :: [Text])
    --   pure ()
    pure ()
  pure ()

body ::
  forall js t m r.
  (ObeliskWidget js t (R FrontendRoute) m) =>
  RoutedT t r m ()
body = do
  routeLink (FrontendRoute_Review :/ ()) $ text "review"
  el "div" $ text "include hero? Note: does nothing right now..."
  includeHeroD <- _inputElement_checked <$> checkBox
  rec tableWidget $ currGameState <$> gameTreeD
      filterBetD <- betBtns initRangeState $ updated (currGameState <$> gameTreeD)
      nodeLockEv <- lockNodeBtn filterBetD
      gameTreeD <-
        foldDyn
          ($)
          (mkRoot initRangeState)
          (leftmost [accNodesFun <$> nodeLockEv, const <$> selectNodeEv])
      selectNodeEv <- switchHold never =<< dyn (treeView filterBetD <$> gameTreeD)
  nodeQueryResponseD <-
    getCurrentNode
      filterBetD
      (getNodePath <$> gameTreeD)
      includeHeroD
  selectShapedHandD <-
    rangeDisplayWidget
      (nodeQueryResponseD <&> shapedHandRange)
  _ <- holdingDisplay selectShapedHandD (nodeQueryResponseD <&> holdingRange)
  matchedHandsDisplay nodeQueryResponseD
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

    rangeDisplayWidget currRange = do
      selectShapedHandE <- rangeDisplay currRange
      holdDyn (mkPair Ace) selectShapedHandE

    lockNodeBtn filterBetD = el "div" $ do
      lockNodeEv <- button "Lock Node"
      pure $ tagPromptlyDyn filterBetD lockNodeEv

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
