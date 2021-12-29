{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module RangeCalc where

import qualified BasicPrelude as P
import BetWidgets
import Common.Route
import Common.Server.Api
import Control.Arrow ((>>>))
import Control.Lens
  ( (<&>),
    (^.),
  )
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShow)
import Handlers
import Obelisk.Frontend
import Obelisk.Route
import Poker
import Poker.Query.ActionIx
import Poker.Range (Range (Range))
import Reflex.Dom
import Servant.Common.Req
import Poker.History.Types

getCurrentNode ::
  forall t m.
  (MonadWidget t m) =>
  -- |
  Dynamic t (Position, BetAction (IxRange (Amount "USD"))) ->
  -- |
  Dynamic t [(Position, BetAction (IxRange (Amount "USD")))] ->
  Dynamic t Bool ->
  Dynamic t Normalisation ->
  m (Dynamic t NodeQueryResponse)
-- TODO use liftM3 to pull out one more level of function (unwrap the Dyn on the input args)
getCurrentNode nodeFilterD nodePathD includeHeroD normD =
  do
    -- Becuase the filterBetDepends on the game state updating, we use the
    -- filter choice as the source dynamic for requesting the filtered range. It used to
    -- be that the request was made twice since it was the join of the dynamic of the filter bet
    -- and current gamestate, but the filter bet was initialised the frame after the gamestate update
    -- meaning duplicate events.
    argsD <-
      forDynM
        (zipDyn nodeFilterD normD)
        ( \(nodeFilter, norm) -> do
            nodePath <- sample (current nodePathD)
            includeHero <- sample (current includeHeroD)
            -- norm <- sample (current normD)
            pure (nodePath, fst nodeFilter, snd nodeFilter, includeHero, norm)
        )
    queryResponseEvSwitch <- dyn (argsD <&> \args -> uncurry5 getFilterResultD args)
    queryResponseEv <- switchHold never queryResponseEvSwitch
    -- Whenever we make a new query request, we must reset the current query response to
    -- the empty query response, since we no longer know what the node's range is
    -- TODO handle the loading state with a different range viewing widget
    let displayedQueryReponseEv =
          leftmost
            [emptyQueryResponse <$ queryResponseEvSwitch, queryResponseEv]
    holdDyn emptyQueryResponse displayedQueryReponseEv
  where
    uncurry5 f (a, b, c, d, e) = f a b c d e

emptyQueryResponse :: NodeQueryResponse
emptyQueryResponse = NodeQueryResponse [] (Range Map.empty) (Range Map.empty)

getSuccess :: (ReqResult tag a -> Maybe a)
getSuccess res = case res of
  ResponseSuccess _ res _ -> Just res
  ResponseFailure _ txt _ -> Nothing -- traceShow txt $ Nothing
  RequestFailure _ txt -> Nothing -- traceShow txt $ Nothing

handsToRetrieve :: Int
handsToRetrieve = 10

getFilterResultD ::
  (MonadWidget t m) =>
  [(Position, BetAction (IxRange (Amount "USD")))] ->
  Position ->
  BetAction (IxRange (Amount "USD")) ->
  Bool ->
  Normalisation ->
  m (Event t NodeQueryResponse)
getFilterResultD nodePath expectedPos nodeFilter includeHero norm = mdo
  queryRunRes <-
    (backendClient ^. queryApi)
      (Right <$> constDyn (SomeNodeQueryRequest USD $ NodeQueryRequest nodePath includeHero expectedPos nodeFilter norm))
      startUpEv
  startUpEv <- getPostBuild
  pure $ mapMaybe getSuccess queryRunRes
