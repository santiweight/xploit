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

import Control.Lens
import Reflex.Dom
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad
import Poker.Range
import Poker.Base
import Data.List.Split
import Control.Monad.Fix
import qualified Data.Map as Map
import Data.Traversable
import Common.Util

initialRange :: Range ShapedHand Double
initialRange = Range $ Map.empty

rangeCellSize :: Int
rangeCellSize = 100

-- The game board, returns the coordinates of the clicked cell.
mkRangeDisplay
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     , Prerender js t m
     , Reflex t
     )
  => Dynamic t (Range ShapedHand Double) -> m (Event t ShapedHand)
mkRangeDisplay displayRangeD = do
  fmap switchPromptlyDyn . prerender (never <$ blank) $ do
    elAttr "table" (Map.empty) $ do
      el "tbody" $ do
        (fmap $ leftmost . join) . for allRanks $ \r1 ->
          el "tr" $
            for allRanks $ \r2 -> do
              let hand = ranksToShapedHand r1 r2
              (e, _) <- mkRangeCell (lookupOr hand 0 <$> displayRangeD) (tshow hand)
              pure $ hand <$ domEvent Click e

mkRangeCell :: MonadWidget t m => Dynamic t Double -> Text -> m (Element EventResult (DomBuilderSpace m) t,
                        (Element EventResult GhcjsDomSpace t, ()))
mkRangeCell freq cellText = do
            elDynAttr' "td" (cellAttrs <$> freq) $
              elAttr' "div" attrs (cellTable $ cellText)
  where
    cellTable :: MonadWidget t m => Text -> m ()
    cellTable hand = elAttr "div" ("style" =: "font-size: 20px") $ text hand
    attrs :: Map.Map Text Text
    attrs = "style" =: "vertical-align: top"
    cellAttrs :: Double -> Map.Map T.Text T.Text
    cellAttrs freq' =
                  "style" =: T.intercalate ";"
                      [ "overflow: hidden"
                      , "background-image: " <> cellBackground freq'
                      , "white-space: nowrap"
                      , "vertical-align: top"
                      , "width: " <> tshow rangeCellSize <> "px"
                      , "height: " <> tshow rangeCellSize <> "px"
                      ]
    cellBackground :: Double -> T.Text
    cellBackground ((100 -) -> freq') = mconcat
                                        [ "linear-gradient(lightgrey 0%,"
                                        , "lightgrey " <> tshow freq' <> "%,"
                                        , "green " <> tshow freq' <> "%,"
                                        , "green 100%)"
                                        ]


holdingTable :: forall t m. (Reflex t, MonadWidget t m)
              => Dynamic t ShapedHand
              -> Dynamic t (Range Holding Double)
              -> Dynamic t (m (Event t Holding))
holdingTable shD mainRangeD = shD <&> \sh ->
  let rowSize = getRowSize sh
      holdingsRows = chunksOf rowSize . listHoldings $ sh
  in elAttr "table" (Map.empty) $ do
      el "tbody" $ do
        fmap (leftmost . join) . for holdingsRows $ \holdingRow ->
          el "tr" $
            for holdingRow $ \holding -> do
              (e, _) <- mkRangeCell (lookupOr holding 0 <$> mainRangeD) (tshow holding)
              pure $ holding <$ domEvent Click e
  where
    getRowSize (ShapedHand _ shape) = case shape of
                  Offsuit -> 4
                  Suited  -> 2
                  Pair    -> 3
    listHoldings :: ShapedHand -> [Holding]
    listHoldings (ShapedHand (r1, r2) shape) =
      case shape of
        Suited -> [ Holdem (Card r1 suit_) (Card r2 suit_) | suit_ <- listSuit ]
        Pair   -> [ Holdem (Card r1 s1) (Card r2 s2)
                  | s1 <- listSuit
                  , s2 <- listSuit
                  , fromEnum s1 > fromEnum s2
                  ]
        Offsuit   -> [ Holdem (Card r1 s1) (Card r2 s2)
                  | s1 <- listSuit
                  , s2 <- listSuit
                  , s1 /= s2
                  ]

allRanks :: [Rank]
allRanks = reverse listEnum

ranksToShapedHand :: Rank -> Rank -> ShapedHand
ranksToShapedHand rowRank headerRank =
  case compare rowRank headerRank of
      GT -> ShapedHand (rowRank, headerRank) Suited
      EQ -> ShapedHand (rowRank, headerRank) Pair
      LT -> ShapedHand (headerRank, rowRank) Offsuit

lookupOr :: Ord k => k -> a -> Range k a -> a
lookupOr k a (Range m) = Map.lookup k m & maybe a id

infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
