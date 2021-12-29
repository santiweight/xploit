{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Review.Widget where

import qualified BasicPrelude as P
import BetWidgets
import Common.Route
import Common.Server.Api
import Common.Util (tshow)
import Control.Lens
  ( to,
    (^.),
  )
import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.List (unzip4)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import GameTable
import Handlers hiding (postForReview)
import qualified Handlers
import JSDOM (nextAnimationFrame)
import JSDOM.Types (liftJSM, toElement)
import Language.Javascript.JSaddle (JSM, JSVal, ToJSVal (toJSVal), eval, js1, js3, jsgf)
import Money (Approximation (Round), defaultDecimalConf, denseFromDiscrete, denseToDecimal)
import Obelisk.Frontend
  ( ObeliskWidget,
  )
import Obelisk.Route
import Obelisk.Route.Frontend (RoutedT, routeLink)
import Poker
import Poker.Game.Bovada (preflopState)
import Poker.Game.Emulate
import Poker.Game.Types
import Poker.Game.Utils (runGame)
import Poker.History.Bovada.Model
import Poker.History.Bovada.Parser (pHand)
import Poker.History.Types (unsafeToUsdHand)
import Poker.Query.ActionIx (IxRange (ExactlyRn))
import RangeCalc
  ( getCurrentNode,
  )
import RangeDisplay
  ( holdingDisplay,
    rangeDisplayWidget,
  )
import Reflex.Dom.Core
import Servant.Common.Req (ReqResult (RequestFailure, ResponseFailure, ResponseSuccess))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

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
  ResponseFailure {} -> Nothing
  RequestFailure _ _ -> Nothing

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

cardToUnicode :: Card -> Char
cardToUnicode (Card r s) =
  toEnum $
    fromJust (r `Map.lookup` ranksAceIndexed)
      + fromJust (s `Map.lookup` suits)
  where
    ranksAceIndexed = Data.Map.fromList $ flip zip ([0 .. 10] ++ [12, 13, 14]) $ take 13 . (Ace :) $ allRanks
    suits = Data.Map.fromList $zip [Spade, Heart, Diamond, Club] [0x0001F0A1, 0x0001F0B1, 0x0001F0C1, 0x0001F0D1]

cardToSpan c@(Card _ s) = do
  let suitClass = case s of
        Club -> "club-color"
        Diamond -> "diamond-color"
        Heart -> "heart-color"
        Spade -> "spade-color"
  elClass "span" suitClass $ text $ T.singleton (cardToUnicode c)

holdingsWidget reviewHistory = do
  el "div" $
    case holes reviewHistory of
      Nothing -> text "holdings not available"
      Just m ->
        P.forM_ (Map.assocs m) $ \(pos, Hand l r) -> do
          el "div" $ do
            text $ tshow pos <> " "
            cardToSpan l
            cardToSpan r

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
                  holdingsWidget reviewReq
                  _ <- postForReview (constDyn reviewReq) ev
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
                    -- dynText $ tshow <$> currHoverDyn
                    liftJSM $
                      nextAnimationFrame
                        ( \_ -> liftJSM $ do
                            _ <- jsgf ("jQuery" :: Text) [toElement . _element_raw $ e] >>= (^. js1 ("accordion" :: Text) (eval ("({ active: 0 , collapsible: false })" :: Text)))
                            pure ()
                        )
                    -- el "div" $ dynText $ T.concat <$> sequence comments
                    --  let actsWithGss = zip nonPostActs gss
                    ixDyn <- mdo
                      ixDyn <- foo actEvs nonPostActs e
                      pure ixDyn
                    let gsDyn = ixDyn <&> (gss !!)
                    elAttr "div" ("style" =: "padding-top:400px") $ dyn_ $ gsDyn <&> gameTable
                    -- rec rangesResponse :: (Event t (ReqResult () (Map Int (Range Hand Double, Range ShapedHand Double)))) <-
                    --       (backendClient ^. reviewClient . to Handlers.reviewRanges)
                    --         (constDyn $ Right nonPostActs)
                    --         getRangesEv
                    --     rangesOpt <- holdDyn Nothing (fmap Just . mapMaybe getSuccess $ rangesResponse)
                    dyn_ $
                      ixDyn
                        <&> ( \ix -> do
                                prerender (pure ()) $ do
                                  let gs = gss !! ix
                                  let as = take ix $ go nonPostActs
                                  pathRangeWidget gs as
                            )
              )

    -- liftJSM $ nextAnimationFrame $ \_ -> do
    --   _ <- (unElement . toElement $ _element_raw e) ^. jsf ("annotation" :: Text) ([] :: [Text])
    --   pure ()
    pure ()
  pure ()

foo actEvs nonPostActs e = mdo
  ixDyn <- foldDyn ($) 0 (leftmost $ minusOneEv : plusOneEv : (fmap const <$> actEvs))
  ((subtract 1 <$) -> minusOneEv) <- switchHold never =<< dyn (ixDyn <&> \ix -> if ix == 0 then never <$ text "at ix 0" else button "-1")
  (((+ 1) <$) -> plusOneEv) <- switchHold never =<< dyn (ixDyn <&> \ix -> if ix == length nonPostActs then never <$ text ("at ix " <> tshow ix) else button "+1")
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

pathRangeWidget gs as = do
  filterBetD <- betBtns (ExactlyRn <$> gs) never
  responseD <-
    getCurrentNode
      filterBetD
      (constDyn $ fmap (fmap ExactlyRn) <$> as)
      (constDyn True)
  selectShapedHandD <-
    rangeDisplayWidget
      (responseD <&> shapedHandRange)
  _ <- holdingDisplay selectShapedHandD (responseD <&> holdingRange)
  pure ()

go :: [Poker.Game.Types.Action (Amount "USD")] -> [(Position, BetAction (Amount "USD"))]
go bar = case bar of
  [] -> []
  x0 : x1 -> case x0 of
    ac -> case ac of
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
