{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server.RunQuery where

import Common.Server.Api
  ( NodeQueryRequest (..),
    NodeQueryResponse (..),
  )
import Control.Applicative (empty)
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
import Poker
import Poker.Game.Types
  ( EvalState (..),
  )
import Poker.History.Bovada.Model
import Poker.Query.ActionIx
import Poker.Query.Base
import Poker.Query.Eval.Base
import Poker.Query.Eval.Internal
import Polysemy (Sem)
import Polysemy.State (State)
import Prettyprinter
import Prelude hiding (pred)

matchedHandsNum :: Int
matchedHandsNum = 10

getNode :: [History (Amount "USD")] -> NodeQueryRequest -> NodeQueryResponse
getNode hands NodeQueryRequest {..} =
  let handResults =
        catMaybes $ flip (tryGetHandNode $ nodePath ++ [(case nodePath of {[] -> UTG; _ -> succ (fst $ last nodePath)}, undefined)]) includeHero <$> hands
      accRange = joinHandResults . fmap snd $ handResults
      getHandIfFilterMatched (hand, handResultRange) =
        case foldMap
          (Sum . length . filter (doesBetMatch nodeFilter))
          handResultRange of
          0 -> Nothing
          _ -> Just hand
      handsMatchedFilter =
        take matchedHandsNum
          . catMaybes
          $ handResults
            <&> getHandIfFilterMatched
              . second getResultRange
      -- TODO fix ""
      getResultRange r = fromMaybe Map.empty $ r Map.!? show (maybe UTG fst . listToMaybe $ reverse nodePath)
      resultRange = getResultRange accRange
      (holdingRange, shapedHandRange) = getCurrentRanges nodeFilter resultRange
   in NodeQueryResponse {..}
  where
    joinHandResults = Map.unionsWith (Map.unionWith (++))

getCurrentRanges ::
  BetAction (IxRange (Amount "USD")) ->
  Map Hand [BetAction (Amount "USD")] ->
  (Range Hand Double, Range ShapedHand Double)
getCurrentRanges currActFilter nodeRange =
  let currActFilterFunction = doesBetMatch currActFilter
      holdingRange = applyFilterAsFreq currActFilterFunction nodeRange
      utgShowRange =
        applyFilterAsFreq currActFilterFunction
          . holdingRangeToShapedRange
          $ nodeRange
   in (Range holdingRange, Range utgShowRange)
  where
    holdingRangeToShapedRange = Map.mapKeysWith (++) handToShaped

applyFilterAsFreq ::
  (BetAction (Amount "USD") -> Bool) -> Map k [BetAction (Amount "USD")] -> Map k Double
applyFilterAsFreq ix =
  fmap
    ( \acts ->
        100 * (fromIntegral (length (filter ix acts)) / fromIntegral (length acts))
    )

tryGetHandNode ::
  (IsBetSize b, IsBet b, Pretty b, Show b) =>
  [(Position, BetAction (IxRange b))] ->
  History b ->
  Bool ->
  Maybe (History b, Map String (Map Hand [BetAction b]))
tryGetHandNode branch hand includeHero =
  fmap (hand,) . listToMaybe . rights $
    runIxBetsAsQuery
      hand
      -- TODO support hero
      -- includeHero
      branch

-- runIxBetsAsQuery
--   :: forall b
--    . (IsBetSize b, Show b)
--   => Hand b
--   -> Bool
--   -> [(Position, BetAction (IxRange (Amount "USD")))]
--   -> [ Either
--          (Either (GameError b) EvalErr)
--          (Map String (Map Hand [BetAction b]))
--      ]
-- runIxBetsAsQuery hand includeHero =
--   fmap flattenErrors
--     . filter filterEmpty
--     . (fmap . fmap) (second _accRanges)
--     . runSem (toEvalState hand)
--     . flip (compileBetsToSem @b) includeHero

-- resultRangeName = "result"

-- -- TODO avoid usage of accumulate
-- compileBetsToSem
--   :: forall b
--    . (Is(Amount "USD") b, Show b)
--   => [(Position, BetAction (IxRange (Amount "USD")))]
--   -> Bool
--   -> Sem (State (EvalEnv b (SemTypes b)) : SemTypes b) ()
-- compileBetsToSem [] includeHero = do
--   nextAct <- next
--   let includeBet = case isHero nextAct of
--         Hero    -> includeHero
--         Villain -> True
--   when includeBet $ accumulate @b resultRangeName nextAct
-- compileBetsToSem ((ixPos, ixAct) : ixs) includeHero = do
--   playerAct@(PlayerAction pos act _) <- next @b
--   if doesBetMatch ixAct act && pos == ixPos
--     then accumulate @b (show pos) playerAct >> compileBetsToSem ixs includeHero
--     else empty
