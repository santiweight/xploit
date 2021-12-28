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

import qualified BasicPrelude as P
import BetWidgets
import Common.Route
import Common.Server.Api
import Common.Util (tshow)
import Control.Arrow ((>>>))
import Control.Lens
  ( snoc,
    to,
    (^.),
  )
import Control.Monad (forM, join)
import Data.Functor ((<&>))
import Data.List (unzip4)
import Data.Map (Map, mapWithKey)
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import GameLogic
import GameTable
import Handlers
import qualified Head
import JSDOM (nextAnimationFrame)
import JSDOM.Types (Element (unElement), liftJSM, toElement)
import Language.Javascript.JSaddle (JSM, JSVal, ToJSVal (toJSVal), eval, js0, js1, js3, jsf, jsgf, val)
import Money (Approximation (Round), defaultDecimalConf, denseFromDiscrete, denseToDecimal)
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
import Servant.Common.Req (ReqResult (RequestFailure, ResponseFailure, ResponseSuccess))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Poker.Query.ActionIx (IxRange(ExactlyRn))

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

getSuccess :: (ReqResult tag a -> Maybe a)
getSuccess res = case res of
  ResponseSuccess _ res _ -> Just res
  ResponseFailure _ txt _ -> Nothing
  RequestFailure _ txt -> Nothing

postForReview ::
  (MonadWidget t m) =>
  Dynamic t ReviewHistory ->
  Event t () ->
  m (Event t ())
postForReview reviewHist trigger = do
  queryRunRes <-
    (backendClient ^. reviewClient . to Handlers.postForReview)
      (Right <$> reviewHist)
      trigger
  pure $ mapMaybe getSuccess queryRunRes

compose :: Ord b => Map b c -> Map a b -> Map a c
compose bc !ab
  | null bc = Map.empty
  | otherwise = mapMaybe (bc Map.!?) ab

review ::
  forall js t m r.
  (ObeliskWidget js t (R FrontendRoute) m) =>
  RoutedT t r m ()
review = mdo
  el "div" $ routeLink (FrontendRoute_Main :/ ()) $ text "home"
  let config = def & textAreaElementConfig_initialValue .~ defaultHand & initialAttributes .~ ("rows" =: "20" <> "cols" =: "90")
  inputEl <- el "div" $ textAreaElement config
  let inputTxtDyn = _textAreaElement_value inputEl
  let parsedHandDyn = parse pHand "" <$> inputTxtDyn
  prerender_ (text "rendering") $
    dyn_ $
      parsedHandDyn
        <&> ( \case
                Left peb -> do
                  el "div" $ text "cannot parse:"
                  el "div" $ text $ T.pack $ errorBundlePretty peb
                Right (fmap unsafeToUsdHand -> his) -> do
                  let posToPlayer = compose (_handPlayerMap his) (_handSeatMap his)
                  let reviewReq =
                        ReviewHistory
                          (sequence $ _playerHolding <$> posToPlayer)
                          (Stack . _stack <$> posToPlayer)
                          []
                  ev <- el "div" $ button "submit"
                  text $ tshow reviewReq
                  Frontend.postForReview (constDyn reviewReq) ev
                  pure ()
            )
  prerender_ (text "rendering") $ do
    el "div" $
      dyn_ $
        parsedHandDyn
          <&> ( \case
                  Left peb -> text $ tshow peb
                  Right his -> do
                    let usdHist = fmap unsafeToUsdHand his
                    let (preflopHand, nonPostActs) = preflopState usdHist
                    let gss = P.scanl' (\gs act -> either (error . show) id $ runGame (emulateAction act) gs) preflopHand nonPostActs
                    (e, unzip4 -> (actEvs, mouseOverEvs, mouseLeaveEvs, comments)) <-
                      elAttr "div" ("class" =: "foo" <> "style" =: "width:350px") $ do
                        el' "div" $ do
                          forM (zip [0 ..] nonPostActs) $ \(ix, act) -> do
                            (el_, _) <- el' "h3" $ text $ toShortText act
                            commentArea <- el "div" $ textAreaElement def
                            pure (ix <$ domEvent Click el_, ix <$ domEvent Mouseover el_, ix <$ domEvent Mouseleave el_, _textAreaElement_value commentArea)
                    let mouseOverEv = leftmost mouseOverEvs
                    let mouseLeaveEv = leftmost mouseLeaveEvs
                    currHoverDyn <- holdDyn Nothing (leftmost [Just <$> mouseOverEv, Nothing <$ mouseLeaveEv])
                    dynText $ tshow <$> currHoverDyn
                    liftJSM $
                      nextAnimationFrame
                        ( \_ -> liftJSM $ do
                            _ <- jsgf ("jQuery" :: Text) [toElement . _element_raw $ e] >>= (^. js1 ("accordion" :: Text) (eval ("({ active: 0 , collapsible: false })" :: Text)))
                            pure ()
                        )
                    el "div" $ dynText $ T.concat <$> sequence comments
                    --  let actsWithGss = zip nonPostActs gss
                    ixDyn <- mdo ixDyn <- foldDyn ($) 0 (leftmost $ minusOneEv : plusOneEv : (fmap const <$> actEvs))
                                 ((subtract 1 <$) -> minusOneEv) <- switchHold never =<< dyn (ixDyn <&> \ix -> if ix == 0 then never <$ text "at ix 0" else button $ "-1")
                                 (((+ 1) <$) -> plusOneEv) <- switchHold never =<< dyn (ixDyn <&> \ix -> if ix == length nonPostActs then never <$ text ("at ix " <> tshow ix) else button "+1")
                                 let gsDyn = ixDyn <&> \ix -> gss !! ix
                                 elAttr "div" ("style" =: "padding-top:400px") $ dyn_ $ gsDyn <&> gameTable
                                 performEvent_ $
                                   tagPromptlyDyn ixDyn (leftmost [minusOneEv, plusOneEv]) <&> \curr -> do
                                     liftJSM $
                                       nextAnimationFrame
                                         ( \_ -> liftJSM $ do
                                             newIx <- (toJSVal (curr :: Int) :: JSM JSVal)
                                             _ <- jsgf ("jQuery" :: Text) [toElement . _element_raw $ e] >>= (^. js3 ("accordion" :: Text) (eval ("\"option\"" :: Text)) (eval ("\"active\"" :: Text)) newIx)
                                             pure ()
                                         )
                                 pure ixDyn
                    rec rangesResponse :: (Event t (ReqResult () (Map Int (Range Hand Double, Range ShapedHand Double)))) <-
                          (backendClient ^. reviewClient . to Handlers.reviewRanges)
                            (constDyn $ Right nonPostActs)
                            getRangesEv
                        rangesOpt <- holdDyn Nothing (fmap Just . mapMaybe getSuccess $ rangesResponse)
                        el "div" $
                          dyn_ $
                            rangesOpt
                              <&> ( \case
                                      Nothing -> text "nothing loaded yet"
                                      Just m -> dyn_ $ ixDyn <&> (\ix -> do
                                        prerender (pure ()) $ do
                                          sequence_ $ do
                                            Map.assocs m <&> (\(num, (shapedRange, holdingRange)) -> do
                                              el "div" $ text (tshow num)
                                              -- rangeDisplay (constDyn holdingRange)
                                              pure () )
                                          filterBetD <- betBtns (fmap ExactlyRn $ gss !! ix) never
                                          responseD <- getCurrentNode
                                            filterBetD
                                            (constDyn $ fmap (fmap (fmap ExactlyRn)) $ take ix $ go $ nonPostActs)
                                            (constDyn True)
                                          selectShapedHandD <-
                                            rangeDisplayWidget
                                              (responseD <&> shapedHandRange)
                                          _ <- holdingDisplay selectShapedHandD (responseD <&> holdingRange)
                                          pure ())
                                  )
                        getRangesEv <- button "getRanges"

                    pure ()
              )

    -- liftJSM $ nextAnimationFrame $ \_ -> do
    --   _ <- (unElement . toElement $ _element_raw e) ^. jsf ("annotation" :: Text) ([] :: [Text])
    --   pure ()
    pure ()
  pure ()

go :: [Poker.Game.Types.Action (Amount "USD")] -> [(Position, BetAction (Amount "USD"))]
go bar = case bar of
  [] -> []
  x0 : x1 -> case x0 of
    (ac) -> case ac of
      Poker.Game.Types.MkPlayerAction pa -> case pa of
        Poker.Game.Types.PlayerAction po ba -> (po, ba) : go x1
      Poker.Game.Types.MkDealerAction da -> []
      Poker.Game.Types.MkPostAction pa -> go x1

toShortText :: Poker.Game.Types.Action (Amount "USD") -> Text
toShortText (MkPlayerAction pav) = case pav of
  Poker.Game.Types.PlayerAction po ba ->
    tshow po <> " " <> case ba of
      Call am -> "calls " <> amtToText am
      Raise am am' -> "raises to " <> amtToText am'
      AllInRaise am am' -> "raises all in to " <> amtToText am'
      Bet am -> "bets " <> amtToText am
      AllIn am -> "calls all in " <> amtToText am
      Fold -> "folds"
      Check -> "checks"
toShortText (Poker.Game.Types.MkDealerAction da) = "TODO"
toShortText (MkPostAction pa) = "TODO"

amtToText :: Amount "USD" -> Text
amtToText (unAmount -> amt) = "$" <> denseToDecimal defaultDecimalConf Round (denseFromDiscrete amt)

body ::
  forall js t m r.
  (ObeliskWidget js t (R FrontendRoute) m) =>
  RoutedT t r m ()
body = prerender_ (pure ()) $ do
  routeLink (FrontendRoute_Review :/ ()) $ text "review"
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
  nodeQueryResponseD <-
    getCurrentNode
      filterBetD
      (getNodePath <$> gameTreeD)
      includeHeroD
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

rangeDisplayWidget currRange = do
  selectShapedHandE <- rangeDisplay currRange
  holdDyn (mkPair Ace) selectShapedHandE

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
