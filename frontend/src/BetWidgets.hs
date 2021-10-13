{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BetWidgets where

import qualified BasicPrelude as P
import Common.Route
import Common.Util (tshow)
import Control.Lens
  ( (<&>),
    (^.),
  )
import Control.Monad
import Control.Monad.Fix
import Control.Monad.State (evalStateT)
import Data.Bifunctor
import qualified Data.Text as T
import GameLogic ()
import Money (discrete, mkSomeDiscrete, someDiscreteAmount, toSomeDiscrete)
import Obelisk.Frontend
import Obelisk.Route
import Poker
import Poker.Game.AvailableActions (AvailableAction (..), availableActions)
import Poker.Game.Types
import Poker.Query.ActionIx
import Reflex.Dom
import Slider

data SBetAction = SCheck | SFold | SCall | SRaise | SAllInRaise
  deriving (Show, Eq)

data RangeType = AboveRnS | BelowRnS | BetweenRnS | ExactlyRnS | AnyRnS
  deriving (Eq, Ord)

mkRangeTyDropdown ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  m (Dropdown t RangeType)
mkRangeTyDropdown =
  dropdown
    BetweenRnS
    (constDyn opts)
    (def & dropdownConfig_attributes .~ constDyn ("class" =: "bet-type-dropdown"))
  where
    opts =
      BetweenRnS
        =: "between"
        <> ExactlyRnS
        =: "exactly"
        <> AboveRnS
        =: "above"
        <> BelowRnS
        =: "below"
        <> AnyRnS
        =: "any"

toSing :: Show t => BetAction t -> SBetAction
toSing = \case
  Check -> SCheck
  Call _ -> SCall
  Raise _ _ -> SRaise
  Fold -> SFold
  other -> error $ "not yet supported: " ++ show other

betBtns ::
  (ObeliskWidget js t r m) =>
  GameState (IxRange (Amount "USD")) ->
  Event t (GameState (IxRange (Amount "USD"))) ->
  m (Dynamic t (Position, BetAction (IxRange (Amount "USD"))))
betBtns initialGameState gameStateEv =
  join
    <$> widgetHold
      (betBtnsInner initialGameState)
      (betBtnsInner <$> gameStateEv)

betBtnsInner ::
  (ObeliskWidget js t r m) =>
  GameState (IxRange (Amount "USD")) ->
  m (Dynamic t (Position, BetAction (IxRange (Amount "USD"))))
betBtnsInner gameState = do
  -- TODO remove unsafe head usage
  let pos :: Position = head $ _toActQueue gameState
  let availActs = [ACall @(IxRange (Amount "USD")) AnyRn, AFold, ACheck, ABet AnyRn AnyRn, ARaiseBetween AnyRn AnyRn]
  text $ tshow $ availActs
  elClass "div" "bet-buttons-container" $ mdo
    betEvs <- forM availActs $ \availAct -> betBtnOf pos (fromAvailAct availAct) chosenBetDyn
    chosenBetDyn <- holdDyn SFold $ toSing . snd <$> leftmost betEvs
    holdDyn (pos, Fold) $ leftmost betEvs

-- case availableActions gameState of
--   Left  _            -> error "no available acts" -- [] <$ text "no available acts"
--   Right availActsMay -> case availActsMay of
--     (pos, availActs) -> elClass "div" "bet-buttons-container" $ mdo
--       betEvs <- forM availActs $ \availAct -> betBtnOf pos (fromAvailAct availAct) chosenBetDyn
--       chosenBetDyn <- holdDyn SFold $ toSing . snd <$> leftmost betEvs
--       holdDyn (pos, Fold) $ leftmost betEvs

fromAvailAct :: AvailableAction (IxRange (Amount "USD")) -> BetAction (IxRange (Amount "USD"))
fromAvailAct (ACall ir) = Call ir
fromAvailAct (ARaiseBetween ir ir') = Raise mempty ir'
fromAvailAct (ARaiseAllIn ir) = AllInRaise mempty ir
fromAvailAct (AAllIn ir) = AllIn ir
fromAvailAct AFold = Fold
fromAvailAct ACheck = Check
fromAvailAct (ABet ir ir') = Bet ir'

amountToInt = fromIntegral . someDiscreteAmount . toSomeDiscrete . unAmount

betBtnOf ::
  (ObeliskWidget js t r m) =>
  Position ->
  BetAction (IxRange (Amount "USD")) ->
  Dynamic t SBetAction ->
  m (Event t (Position, BetAction (IxRange (Amount "USD"))))
betBtnOf pos a currBetSDyn = do
  -- numInput <- inputElement (def & inputElementConfig_initialValue .~ "0" & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number"))
  -- display $ _inputElement_value numInput
  -- sliderEv <- singleSlider (0, 10)
  -- sliderVals <- holdDyn 5 sliderEv
  -- display sliderVals
  case a of
    Check -> ((pos, Check) <$) <$> betButton "Check" SCheck
    bet@(Call _) -> ((pos, bet) <$) <$> betButton "Call" SCall
    Fold -> ((pos, Fold) <$) <$> betButton "Fold" SFold
    bet@(AllInRaise (ExactlyRn _) (ExactlyRn amt)) ->
      ((pos, bet) <$) <$> button ("Raise All-In" <> P.tshow amt)
    Raise _ (BetweenRn (amountToInt -> lo) (amountToInt -> hi)) -> elClass "div" "raiseWidget" $ do
      raiseBtnEv <- betButton "raise" SRaise
      rangeTyDropdown <- mkRangeTyDropdown
      let rangeTyDyn = rangeTyDropdown ^. dropdown_value
      amt <-
        dyn $
          rangeTyDyn <&> \case
            -- TODO should we ensure that switching the dropdown for a bet size triggers the new slider val event?
            -- Currently the event is tagged by the clicking of the raise button, because tagging the event
            -- on slider click would be involve too much runtime. Options:
            --  - run on slider release/text input in input box
            --  - run on enter for input box?
            --  - keep it dependent on raise button click but style button to draw attention to the issue
            ExactlyRnS -> singleSliderWidget ExactlySlider (lo, hi) ExactlyRn
            BetweenRnS -> mdo
              sliderEv <- elClass "div" "between-slider-container" $ do
                el "div" $ display loVal
                sliderEv_ <- doubleSlider (lo, hi)
                display hiVal
                pure sliderEv_
              sliderVals <- holdDyn (lo, hi) sliderEv
              let (loVal, hiVal) = splitDynPure sliderVals
              pure $ uncurry BetweenRn . bimap (unsafeMkAmount . discrete . fromIntegral) (unsafeMkAmount . discrete . fromIntegral) <$> sliderEv
            BelowRnS -> singleSliderWidget BelowSlider (lo, hi) BelowRn
            AboveRnS -> singleSliderWidget AboveSlider (lo, hi) AboveRn
            AnyRnS -> getPostBuild <&> fmap (const AnyRn)
      amtDyn <-
        holdDyn (BetweenRn (unsafeMkAmount 50) (unsafeMkAmount 250) :: IxRange (Amount "USD")) =<< switchHold never amt
      let raiseEv =
            tagPromptlyDyn ((pos,) . Raise (ExactlyRn $ unsafeMkAmount 0) <$> amtDyn) raiseBtnEv
      pure raiseEv
    other -> never <$ (text . T.pack $ show other)
  where
    singleSliderWidget sliderTy (lo, hi) sizeToRangeFun =
      elClass "div" "single-slider-container" $ mdo
        display sliderVal
        sliderEv <- singleSlider sliderTy (lo, hi)
        sliderVal <- holdDyn (fromRational $ toRational lo + (toRational hi / 2)) (fromIntegral <$> sliderEv)
        pure $ sizeToRangeFun . unsafeMkAmount . discrete . fromIntegral <$> sliderEv
    betButton txt betTyS = do
      (raiseBtnEl, _) <- elDynAttr' "button" buttonAttrs $ text txt
      pure $ domEvent Click raiseBtnEl
      where
        isActiveDyn =
          currBetSDyn <&> \currBetS ->
            if betTyS == currBetS then " pure-button-active" else ""
        buttonAttrs = do
          isActive <- isActiveDyn
          pure
            ("class" =: ("pure-button" <> isActive) <> "style" =: "display:block")
