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
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module RangeDisplay where

import           BasicPrelude                   ( tshow )
import qualified BasicPrelude                  as P
import           Common.Route                   ( FrontendRoute )
import           Control.Applicative            ( Const(Const) )
import           Control.Lens                   ( Const
                                                , Identity(Identity)
                                                )
import           Control.Monad
-- import Data.List.Split
import           Control.Monad.Fix
import           Data.Functor                   ( (<&>) )
import           Data.List.Extra                ( chunksOf )
import qualified Data.Map                      as Map
import           Data.Semigroup
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Traversable
import           Obelisk.Frontend
import           Obelisk.Route                  ( R )
import           Poker
import           Poker.Range
import           Reflex.Dom              hiding ( Alt )
import           Util                           ( execEventWriterT, execFirstEventWriterT )
import Prettyprinter (Pretty (pretty), layoutCompact)
import Prettyprinter.Render.Text
-- import Common.Util

initialRange :: Range ShapedHand Double
initialRange = Range Map.empty

rangeCellSize :: Int
rangeCellSize = 50

rangeDisplay
  :: (MonadWidget t m)
  => Dynamic t (Range ShapedHand Double)
  -> m (Event t ShapedHand)
rangeDisplay displayRangeD = execFirstEventWriterT $ do
  elClass "table" "range-table" $ el "tbody" $ forM_ allRanks $ \r1 ->
    el "tr" $ for allRanks $ \r2 -> do
      let hand = ranksToShapedHand r1 r2
      tellEvent =<< rangeCell hand displayRangeD
 where
  listEnum = enumFrom (toEnum 0)
  allRanks = reverse listEnum
  ranksToShapedHand rowRank headerRank = case compare rowRank headerRank of
    GT -> MkSuited rowRank headerRank
    EQ -> MkPair rowRank
    LT -> MkOffsuit headerRank rowRank

holdingDisplay
  :: _
  => Dynamic t ShapedHand
  -> Dynamic t (Range Hand Double)
  -> m (Event t Hand)
holdingDisplay shapedHandD mainRangeD = execFirstEventWriterT $ do
  elClass "table" "range-table" $ do
    el "tbody" $ do
      dyn_ $ shapedHandD <&> \shapedHand -> do
        forM_ (getHandRows shapedHand) $ \row -> do
          el "tr" $ for row $ \holding -> do
            tellEvent =<< rangeCell holding mainRangeD
 where
  getHandRows shapedHand =
    chunksOf (getRowSize shapedHand) . listHands $ shapedHand
  getRowSize = \case
    MkOffsuit _ _ -> 4
    MkSuited _ _ -> 2
    MkPair    _ -> 3
  listHands :: ShapedHand -> [Hand]
  listHands = \case
    MkSuited r1 r2 -> [ MkHand (Card r1 suit_) (Card r2 suit_) | suit_ <- allSuits ]
    MkPair r ->
      [ MkHand (Card r s1) (Card r s2)
      | s1 <- allSuits
      , s2 <- allSuits
      , fromEnum s1 > fromEnum s2
      ]
    MkOffsuit r1 r2 ->
      [ MkHand (Card r1 s1) (Card r2 s2)
      | s1 <- allSuits
      , s2 <- allSuits
      , s1 /= s2
      ]

prettyText :: Pretty a => a -> Text
prettyText = renderStrict . layoutCompact . pretty

rangeCell
  :: (DomBuilder t m, MonadWidget t m, Pretty k, Ord k)
  => k
  -> Dynamic t (Range k Double)
  -> m (Event t (First k))
rangeCell hand rangeD = do
  let freq = lookupOr hand 0 <$> rangeD
  (e, _) <- elDynAttr' "td" (cellAttrs <$> freq) $ text (prettyText hand)
  pure $ First hand <$ domEvent Click e
 where
  cellAttrs freq' =
    "style"
      =: ("background-image: " <> cellBackground freq')
      <> "class"
      =: "range-cell"
  cellBackground ((100 -) -> freq') = mconcat
    [ "linear-gradient(lightgrey 0.0%,"
    , "lightgrey " <> tshow freq' <> "%,"
    , "green " <> tshow freq' <> "%,"
    , "green 100%)"
    ]
  lookupOr k a (Range m) = Map.lookup k m & P.fromMaybe a


