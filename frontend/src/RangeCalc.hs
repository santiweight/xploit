{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Poker.Base
import           Poker.Range                    ( Range(Range) )
import           Reflex.Dom
import           Servant.Common.Req

getCurrentNode
  :: (ObeliskWidget js t (R FrontendRoute) m)
  => Dynamic t (Position, BetAction (IxRange BetSize)) -- ^
  -> Dynamic t [(Position, BetAction (IxRange BetSize))] -- ^
  -> Dynamic t Bool
  -> m (Dynamic t NodeQueryResponse)
-- TODO use liftM3 to pull out one more level of function (unwrap the Dyn on the input args)
getCurrentNode nodeFilterD nodePathD includeHeroD =
  fmap join . prerender (pure $ constDyn emptyQueryResponse) $ do
    queryResponseEvSwitch <- dyn
      $ getResultE nodePathD (snd <$> nodeFilterD) includeHeroD
    queryResponseEv <- switchHold never queryResponseEvSwitch
    -- Whenever we make a new query request, we must reset the current query response to
    -- the empty query response, since we no longer know what the node's range is
    -- TODO handle the loading state with a different range viewing widget
    let displayedQueryReponseEv = leftmost
          [emptyQueryResponse <$ queryResponseEvSwitch, queryResponseEv]
    holdDyn emptyQueryResponse displayedQueryReponseEv
  where getResultE = liftM3 getFilterResultD

emptyQueryResponse :: NodeQueryResponse
emptyQueryResponse = NodeQueryResponse [] (Range Map.empty) (Range Map.empty)


getSuccess :: (ReqResult tag a -> Maybe a)
getSuccess = \case
  ResponseSuccess _ res _ -> Just res
  ResponseFailure{}       -> Nothing
  RequestFailure _ _      -> Nothing

handsToRetrieve :: Int
handsToRetrieve = 10

getFilterResultD
  :: (MonadWidget t m)
  => [(Position, BetAction (IxRange BetSize))]
  -> BetAction (IxRange BetSize)
  -> Bool
  -> m (Event t NodeQueryResponse)
getFilterResultD nodePath nodeFilter includeHero = mdo
  queryRunRes <- (backendClient ^. queryApi)
    (Right <$> constDyn (NodeQueryRequest { .. }))
    startUpEv
  startUpEv <- getPostBuild
  pure $ mapMaybe getSuccess queryRunRes

