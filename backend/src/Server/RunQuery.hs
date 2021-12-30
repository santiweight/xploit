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
import Control.Lens.TH
import Control.Monad
import Data.Bifunctor (Bifunctor (second))
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    listToMaybe,
    mapMaybe,
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

makeLenses ''Range

getNode :: (KnownSymbol b, GoodScale (CurrencyScale b)) => [History (Amount b)] -> NodeQueryRequest b -> NodeQueryResponse
getNode hists NodeQueryRequest {..} =
  let handResults =
        mapMaybe (\hist -> tryGetHandNode nodeExpectedPos nodePath hist includeHero) hists
      resultRangesByHistoryId :: [(HistoryId, Range Hand Freq)] =
        handResults <&> second (getMatchedRange nodeFilter . getResultRange)
      resultRange' = foldr merge mempty $ snd <$> resultRangesByHistoryId
      getHistoryIdIfFilterMatched (hist, Range r) =
        if any (\(Freq num _) -> num > 0) $ Map.elems r
          then Nothing
          else Just hist
      handsMatchedFilter = catMaybes $ resultRangesByHistoryId <&> getHistoryIdIfFilterMatched
      (holdingRange, shapedHandRange) = (freqToDouble <$> resultRange', freqToDouble <$> holdingRangeToShapedRange resultRange')
   in NodeQueryResponse {..}
  where
    getResultRange r = fromMaybe mempty $ r Map.!? show (length nodePath)
    getMatchedRange ::
      IsBet b =>
      BetAction (IxRange b) ->
      Range Hand [BetAction b] ->
      Range Hand Freq
    getMatchedRange nodeFilterAct nodeRange = countFreq (doesBetMatch nodeFilterAct) <$> nodeRange

getMatchedRanges ::
  IsBet b =>
  BetAction (IxRange b) ->
  Range Hand [BetAction b] ->
  (Range Hand Double, Range ShapedHand Double)
getMatchedRanges nodeFilterAct nodeRange =
  let handRange = countFreq (doesBetMatch nodeFilterAct) <$> nodeRange
      shapedHandRange = holdingRangeToShapedRange handRange
   in (freqToDouble <$> handRange, freqToDouble <$> shapedHandRange)

historyId = HistoryId . header

tryGetHandNode ::
  (IsBet b, Pretty b) =>
  Position ->
  [(Position, BetAction (IxRange b))] ->
  History b ->
  Bool ->
  Maybe (HistoryId, Map String (Range Hand [BetAction b]))
tryGetHandNode expectedPos branch hand includeHero =
  -- TODO instead of listToMaybe, could choose the longest match
  fmap ((historyId hand,) . fmap Range) . listToMaybe . rights $
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
      ranges = Map.mapWithKey (getMatchedRanges . (Map.!) ixBetsByActNum) (fmap Range accRange)
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
