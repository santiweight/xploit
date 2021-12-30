{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server.RunQuery where

import Common.Server.Api
  ( HistoryId (HistoryId),
    NodeQueryRequest (..),
    NodeQueryResponse (..),
  )
import Control.Lens ((<&>))
import Control.Monad
import Data.Bifunctor (Bifunctor (second))
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    listToMaybe,
  )
import Data.Monoid (Sum (Sum))
import GHC.TypeLits
import Money
import Poker
import Poker.History.Bovada.Model
import Poker.Query.ActionIx
import Poker.Query.Eval.Base
import Prettyprinter
import Prelude hiding (pred)

matchedHandsNum :: Int
matchedHandsNum = 10

getNode :: (KnownSymbol b, GoodScale (CurrencyScale b)) => [History (Amount b)] -> NodeQueryRequest b -> NodeQueryResponse
getNode hands NodeQueryRequest {..} =
  let expectedPos = nodeExpectedPos
      handResults =
        catMaybes $
          flip
            ( tryGetHandNode expectedPos nodePath
            )
            includeHero
            <$> hands
      accRange = joinHandResults . fmap snd $ handResults
      getHistoryIdIfFilterMatched (hist, handResultRange) =
        case foldMap
          (Sum . length . filter (doesBetMatch nodeFilter))
          handResultRange of
          0 -> Nothing
          _ -> Just $ historyId hist
      handsMatchedFilter =
        take matchedHandsNum
          . catMaybes
          $ handResults
            <&> getHistoryIdIfFilterMatched
              . second getResultRange
      -- TODO fix ""
      -- FIXME This call to succ is non-total and is a hacky way to recover the current node's position. A better way
      -- to do this is to have all players' positions so we can see who will act next. Alternatively, have the ranges accumulate
      -- by the action's index (0..) in the list of all actions.
      getResultRange r = fromMaybe Map.empty $ r Map.!? show (length nodePath)
      resultRange = getResultRange accRange
      (holdingRange, shapedHandRange) = getCurrentRanges nodeFilter (Range resultRange)
   in NodeQueryResponse {..}
  where
    historyId = HistoryId . header
    joinHandResults = Map.unionsWith (Map.unionWith (++))

getCurrentRanges ::
  IsBet b =>
  BetAction (IxRange b) ->
  Range Hand [BetAction b] ->
  (Range Hand Double, Range ShapedHand Double)
getCurrentRanges currActFilter nodeRange =
  let currActFilterFunction = doesBetMatch currActFilter
      holdingRange = applyFilterAsFreq currActFilterFunction (_range nodeRange)
      utgShowRange =
        applyFilterAsFreq currActFilterFunction
          . holdingRangeToShapedRange
          $ (_range nodeRange)
   in (holdingRange, utgShowRange)
  where
    holdingRangeToShapedRange = Map.mapKeysWith (++) handToShaped

applyFilterAsFreq ::
  (BetAction b -> Bool) -> Map k [BetAction b] -> Range k Double
applyFilterAsFreq ix = Range .
  fmap
    ( \acts ->
        100 * (fromIntegral (length (filter ix acts)) / fromIntegral (length acts))
    )

tryGetHandNode ::
  (IsBetSize b, IsBet b, Pretty b, Show b) =>
  Position ->
  [(Position, BetAction (IxRange b))] ->
  History b ->
  Bool ->
  Maybe (History b, Map String (Map Hand [BetAction b]))
tryGetHandNode expectedPos branch hand includeHero =
  fmap (hand,) . listToMaybe . rights $
    getPathRange
      hand
      expectedPos
      -- TODO support hero
      -- includeHero
      branch

getReviewRanges ::
  (Pretty b, IsBet b) =>
  [(Position, BetAction (IxRange b))] ->
  [History b] ->
  Map Int (Range Hand Double, Range ShapedHand Double)
getReviewRanges nodePath hands =
  let handResults =
        catMaybes $
          flip
            (tryGetReviewRanges nodePath)
            False
            <$> hands
      accRange = joinHandResults . fmap snd $ handResults
      ixBetsByActNum = Map.fromList $ zip [0 ..] (snd <$> nodePath)
      -- TODO fix ""
      -- FIXME This call to succ is non-total and is a hacky way to recover the current node's position. A better way
      -- to do this is to have all players' positions so we can see who will act next. Alternatively, have the ranges accumulate
      -- by the action's index (0..) in the list of all actions.
      ranges = Map.mapWithKey (getCurrentRanges . (Map.!) ixBetsByActNum) (fmap Range accRange)
   in ranges
  where
    joinHandResults = Map.unionsWith (Map.unionWith (++))

tryGetReviewRanges ::
  (IsBetSize b, IsBet b, Pretty b, Show b) =>
  [(Position, BetAction (IxRange b))] ->
  History b ->
  Bool ->
  Maybe (History b, Map Int (Map Hand [BetAction b]))
tryGetReviewRanges branch hand includeHero =
  fmap (hand,) . fmap (Map.mapKeys read) . listToMaybe . rights $
    getRangesForPath
      hand
      -- TODO support hero
      -- includeHero
      branch
