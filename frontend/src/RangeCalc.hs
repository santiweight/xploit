{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeFamilies #-}
module RangeCalc where

import qualified BasicPrelude                  as P
import           BetWidgets
import           Common.Route
import           Common.Server.Api
import           Control.Arrow                  ( (>>>) )
import           Control.Lens                   ( (<&>)
                                                , (^.)
                                                )
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Handlers
import           Obelisk.Frontend
import           Obelisk.Route
import           Poker
import           Poker.Range                    ( Range(Range) )
import           Reflex.Dom
import Poker.Query.ActionIx
import           Servant.Common.Req
import Debug.Trace (traceShow)

getCurrentNode
  :: (MonadWidget t m)
  => Dynamic t (Position, BetAction (IxRange (Amount "USD"))) -- ^
  -> Dynamic t [(Position, BetAction (IxRange (Amount "USD")))] -- ^
  -> Dynamic t Bool
  -> m (Dynamic t NodeQueryResponse)
-- TODO use liftM3 to pull out one more level of function (unwrap the Dyn on the input args)
getCurrentNode nodeFilterD nodePathD includeHeroD =
  do
    queryResponseEvSwitch <- dyn
      $ getResultE nodePathD (fst <$> nodeFilterD) (snd <$> nodeFilterD) includeHeroD
    queryResponseEv <- switchHold never queryResponseEvSwitch
    -- Whenever we make a new query request, we must reset the current query response to
    -- the empty query response, since we no longer know what the node's range is
    -- TODO handle the loading state with a different range viewing widget
    let displayedQueryReponseEv = leftmost
          [emptyQueryResponse <$ queryResponseEvSwitch, queryResponseEv]
    holdDyn emptyQueryResponse displayedQueryReponseEv
  where getResultE = liftM4 getFilterResultD

emptyQueryResponse :: NodeQueryResponse
emptyQueryResponse = NodeQueryResponse [] (Range Map.empty) (Range Map.empty)

getSuccess :: (ReqResult tag a -> Maybe a)
getSuccess res = case res of
  ResponseSuccess _ res _ -> Just res
  ResponseFailure _ txt _ -> Nothing -- traceShow txt $ Nothing
  RequestFailure _ txt      -> Nothing -- traceShow txt $ Nothing

handsToRetrieve :: Int
handsToRetrieve = 10

getFilterResultD
  :: (MonadWidget t m)
  => [(Position, BetAction (IxRange (Amount "USD")))]
  -> Position
  -> BetAction (IxRange (Amount "USD"))
  -> Bool
  -> m (Event t NodeQueryResponse)
getFilterResultD nodePath expectedPos nodeFilter includeHero = mdo
  queryRunRes <- (backendClient ^. queryApi)
    (Right <$> constDyn (NodeQueryRequest nodePath includeHero expectedPos nodeFilter))
    startUpEv
  startUpEv <- getPostBuild
  pure $ mapMaybe getSuccess queryRunRes

