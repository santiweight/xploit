{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language MonoLocalBinds #-}

module ActionIxSelector where

import Control.Lens
import Control.Applicative
import Reflex.Dom
import qualified Data.Text as T
import Control.Monad
import Poker
import Poker.Query.ActionIx
import Control.Monad.Fix
import qualified Data.Map as Map
import Money

data IxRangeS
  = AnyRnS
  | BetweenRnS
  | AboveRnS
  | BelowRnS
  deriving (Show, Eq, Ord, Enum)

data ActionIxS
  = AnyIxS
  | RaiseIxS
  | AllInIxS
  | BetIxS
  | RaiseOrAllInIxS
  | CheckIxS
  | CallIxS
  | FoldIxS
  | LeaveIxS
  deriving (Show, Eq, Ord, Enum)

numInput :: MonadWidget t m => m (Dynamic t Double)
numInput = do
      fmap (read . T.unpack) . _inputElement_value <$> inputElement def

actionIxSelector
  :: ( MonadFix m
     , PostBuild t m
     , MonadHold t m
     , MonadWidget t m
     , Prerender js t m
     , Reflex t
     )
  => m (Dynamic t (ActionIx (Amount "USD")))
actionIxSelector = do
    dropdownEl <- dropdown AnyIxS (constDyn rangeIxShowMap) dropdownConfig
    actionIxE :: Event t (Dynamic t (ActionIx (Amount "USD"))) <- dyn $ (mkFullSelector $ _dropdown_value dropdownEl)
    join <$> holdDyn (constDyn AnyIx) actionIxE
  where
    -- mkFullSelector :: (Adjustable t m, NotReady t m, PostBuild t m) => Dynamic t IxRangeS -> Dynamic t (m IxRange)
    mkFullSelector :: (MonadWidget t m, Prerender js t m) => Dynamic t ActionIxS -> Dynamic t (m (Dynamic t (ActionIx (Amount "USD"))))
    mkFullSelector rnSingletonDyn = rnSingletonDyn <&> toSelector
    toSelector :: (MonadWidget t m, Prerender js t m) => ActionIxS -> m (Dynamic t (ActionIx (Amount "USD")))
    toSelector = \case
            AnyIxS -> constDyn AnyIx <$ blank
            RaiseIxS -> fmap RaiseIx <$> rangeIxSelector
            AllInIxS -> fmap AllInIx <$> rangeIxSelector
            BetIxS -> fmap BetIx <$> rangeIxSelector
            RaiseOrAllInIxS -> fmap RaiseOrAllInIx <$> rangeIxSelector
            CheckIxS -> constDyn CheckIx <$ blank
            CallIxS -> constDyn CallIx <$ blank
            FoldIxS -> constDyn FoldIx <$ blank
            LeaveIxS -> constDyn LeaveIx <$ blank

    dropdownConfig = def
    listEnum :: (Enum a) => [a]
    listEnum = enumFrom (toEnum 0)
    rangeIxShowMap :: Map.Map ActionIxS T.Text
    rangeIxShowMap =
      Map.fromList . fmap (liftA2 (,) id toPrettyText) $ listEnum
      where
        toPrettyText :: ActionIxS -> T.Text
        toPrettyText = \case
            AnyIxS -> "Any"
            RaiseIxS -> "Raise"
            AllInIxS -> "All-In"
            BetIxS -> "Bet"
            RaiseOrAllInIxS -> "Raise or All-In"
            CheckIxS -> "Check"
            CallIxS -> "Call"
            FoldIxS -> "Fold"
            LeaveIxS -> "Leave"


rangeIxSelector
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , Reflex t
     , MonadWidget t m
     )
  => m (Dynamic t (IxRange (Amount "USD")))
rangeIxSelector = do
    dropdownEl <- dropdown AnyRnS (constDyn rangeIxShowMap) dropdownConfig
    rangeIxE :: Event t (Dynamic t (IxRange (Amount "USD"))) <- dyn $ (mkFullSelector $ _dropdown_value dropdownEl)
    join <$> holdDyn (constDyn AnyRn) rangeIxE
  where
    -- mkFullSelector :: (Adjustable t m, NotReady t m, PostBuild t m) => Dynamic t IxRangeS -> Dynamic t (m IxRange)
    mkFullSelector :: MonadWidget t m => Dynamic t IxRangeS -> Dynamic t (m (Dynamic t (IxRange (Amount "USD"))))
    mkFullSelector rnSingletonDyn = rnSingletonDyn <&> (fmap . fmap . fmap) (Amount . discrete . truncate . (*100)) . \case
            AnyRnS -> constDyn AnyRn <$ blank
            BetweenRnS -> do
                    dynNum1 <- numInput
                    dynNum2 <- numInput
                    pure (liftA2 BetweenRn dynNum1 dynNum2)
            AboveRnS -> fmap AboveRn <$> numInput
            BelowRnS -> fmap BelowRn <$> numInput

    dropdownConfig = def
    listEnum :: (Enum a) => [a]
    listEnum = enumFrom (toEnum 0)
    rangeIxShowMap :: Map.Map IxRangeS T.Text
    rangeIxShowMap =
      Map.fromList . fmap (liftA2 (,) id toPrettyText) $ listEnum
      where
        toPrettyText :: IxRangeS -> T.Text
        toPrettyText = \case
              AnyRnS -> "Any"
              BetweenRnS -> "Between"
              AboveRnS -> "Above"
              BelowRnS -> "Below"