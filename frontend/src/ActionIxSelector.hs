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
import Data.Aeson (Value (String), FromJSON, parseJSON, genericParseJSON, defaultOptions, fieldLabelModifier)
-- import qualified Reflex.CodeMirror as CM
-- import Reflex.Utils
-- import Reflex.Dom.Widget.Input
-- import Control.Monad.IO.Class
import Control.Monad
-- import Poker.Range
import Poker.Base
-- import Data.List.Split
import Control.Monad.Fix
import qualified Data.Map as Map
-- import           "reflex-utils"   Reflex.Utils
-- import           "reflex-jexcel"  Reflex.JExcel
-- import           "reflex-fileapi" Reflex.FileAPI.FileAPI


-- data IxRange
--   = AnyRn
--   | BetweenRn Double Double
--   | AboveRn Double
--   | BelowRn Double
--   deriving (Show, Eq, Ord, Data, Typeable, Generic)

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

-- data ActionIx
--   = AnyIx
--   | RaiseIx IxRange
--   | AllInIx IxRange
--   | BetIx IxRange
--   | RaiseOrAllInIx IxRange
--   | CheckIx
--   | CallIx
--   | FoldIx
--   | LeaveIx
--   deriving (Show, Eq, Ord, Data, Typeable, Generic)
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
  => m (Dynamic t ActionIx)
actionIxSelector = do
    dropdownEl <- dropdown AnyIxS (constDyn rangeIxShowMap) dropdownConfig
    actionIxE :: Event t (Dynamic t ActionIx) <- dyn $ (mkFullSelector $ _dropdown_value dropdownEl)
    join <$> holdDyn (constDyn AnyIx) actionIxE
  where
    -- mkFullSelector :: (Adjustable t m, NotReady t m, PostBuild t m) => Dynamic t IxRangeS -> Dynamic t (m IxRange)
    mkFullSelector :: (MonadWidget t m, Prerender js t m) => Dynamic t ActionIxS -> Dynamic t (m (Dynamic t ActionIx))
    mkFullSelector rnSingletonDyn = rnSingletonDyn <&> toSelector
    toSelector :: (MonadWidget t m, Prerender js t m) => ActionIxS -> m (Dynamic t ActionIx)
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
  => m (Dynamic t IxRange)
rangeIxSelector = do
    dropdownEl <- dropdown AnyRnS (constDyn rangeIxShowMap) dropdownConfig
    rangeIxE :: Event t (Dynamic t IxRange) <- dyn $ (mkFullSelector $ _dropdown_value dropdownEl)
    join <$> holdDyn (constDyn AnyRn) rangeIxE
  where
    -- mkFullSelector :: (Adjustable t m, NotReady t m, PostBuild t m) => Dynamic t IxRangeS -> Dynamic t (m IxRange)
    mkFullSelector :: MonadWidget t m => Dynamic t IxRangeS -> Dynamic t (m (Dynamic t IxRange))
    mkFullSelector rnSingletonDyn = rnSingletonDyn <&> \case
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