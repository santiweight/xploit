{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Control.Lens
import Control.Monad
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import Language.Javascript.JSaddle (eval, liftJSM)
-- import           "reflex-utils"   Reflex.Utils hiding (whenLoaded)
-- import           "reflex-jexcel"  Reflex.JExcel
-- import           "reflex-fileapi" Reflex.FileAPI.FileAPI
import qualified Data.Map as Map

import Data.List.Split (chunksOf)
import Obelisk.Frontend
-- import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Poker.Base
import Poker.Range

import Reflex.Dom.Core
-- import Data.Bifunctor
-- import Data.Foldable (toList)

import Common.Api
import Common.Route
import "servant-snap" Servant
-- import "servant-snap" Servant.Server
import Servant.Reflex

import QueryInput
import RangeDisplay
import FileInput
import ActionIxSelector

whenLoaded :: forall t m. MonadWidget t m
           => [Dynamic t Bool]
           -- ^ Wait for these to load
           -> m ()
           -- ^ Loading widget
           -> m ()
           -- ^ Loaded widget
           -> m (Dynamic t Bool)
whenLoaded loadedDs
           loadingWidget
           loadedWidget = do
    allLoadedD :: Dynamic t Bool <- return . mconcat $ loadedDs
    -- allLoadedD :: Dynamic t Bool <- return . foldr (liftA2 (&&)) (pure True) $ loadedDs
    let allLoadedE = ffilter id .  updated $ allLoadedD
    delayedLoadedE <- delay 0.05 allLoadedE
    changeD <- widgetHold loadingWidget $ ffor delayedLoadedE $ \_ -> do
        loadedWidget
    changeE_ <- headE $ updated changeD
    let changeE = True <$ changeE_
    holdDyn False changeE

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
        el "div" $ do
          prerender_ blank $ do
            headD <- fileHead
            headCM <- cmHead
            _ <- whenLoaded [headD, headCM] blank $ do
              (codeD, handD) <- mainPane -- pure (queryTextD, constDyn acesHand)
              display codeD
              display handD
            pure ()
  }
    where
      mainPane
        :: forall js t m. (MonadWidget t m, Prerender js t m)
        => m (Dynamic t T.Text, Dynamic t ShapedHand)
      mainPane = do
        resD <- prerender ((constDyn "", constDyn acesHand) <$ text "My Prerender") $
                do
                  rec
                    actionIxD <- join <$> prerender (constDyn AnyIx <$ blank) actionIxSelector
                    let getMainRange :: Dynamic t (Range Holding [PlayerActionValue]
                                          -> Range ShapedHand Double)
                        getMainRange = getDisplayRange <$> actionIxD
                    codeD <- el "div" $ (cmBody =<< fileBody)
                    (handD, fullRangeD) <- el "div" $ do
                                handClickE <- mkRangeDisplay $ getMainRange <*> fullRangeD
                                handD <- holdDyn acesHand handClickE
                                fullRangeD :: Dynamic t (Range Holding [PlayerActionValue])
                                           <- holdDyn (Range Map.empty) successResultE
                                pure (handD, fullRangeD)
                    el "div" $ do
                      dyn $ holdingTable handD (toHoldingFreqs <$> actionIxD <*> fullRangeD)
                      pure ()

                    -- display handD
                    -- display codeD
                    runE <- el "div" $ button "run"
                    resultE <- runQuery (QParamSome . T.unpack <$> codeD) runE
                    let successResultE = mapMaybe reqSuccess resultE
                  pure (codeD, handD)
        pure . bimap join join $ splitDynPure resD
      toHoldingFreqs :: ActionIx -> Range Holding [PlayerActionValue] -> Range Holding Double
      toHoldingFreqs ix mainRange =
        mainRange
              & range . traverse %~ countFreqMatched (inIndex ix)
              & range . traverse %~ freqToDouble


      acesHand :: ShapedHand
      acesHand = ShapedHand (Ace, Ace) Pair
      getActiveSideRange
        :: ShapedHand
        -> ActionIx
        -> Range Holding [PlayerActionValue]
        -> [(Holding, Double)] -- frequencies for holdings under ShapedHand
      getActiveSideRange activeHand actionIx mainR =
        shapeToCombos activeHand <&> \holding ->
          let holdingActs = mainR ^. range . at holding . non []
          in ( holding
             , freqToDouble $ countFreqMatched (inIndex actionIx) holdingActs)
      groupByShape
        :: Range Holding [PlayerActionValue]
        -> Range ShapedHand [PlayerActionValue]
      groupByShape rang = Range . Map.fromList $
            ffor allShapedHands $ \shapedHand ->
              let holdings = shapeToCombos shapedHand
              in ( shapedHand
                 , concatMap
                      (maybe [] id . (`Map.lookup` _range rang))
                      holdings)
      countFreqMatched :: (a -> Bool) -> [a] -> (Int, Int)
      countFreqMatched pred ls =
        (length $ filter pred ls, length ls)
      getDisplayRange
        :: ActionIx
        -> Range Holding [PlayerActionValue]
        -> Range ShapedHand Double
      getDisplayRange ix mainRange =
            groupByShape mainRange
              & range . traverse %~ countFreqMatched (inIndex ix)
              & range . traverse %~ freqToDouble
      freqToDouble :: (Int, Int) -> Double
      freqToDouble (n, d) = fromIntegral n / fromIntegral d * 100



myClient :: forall t m. MonadWidget t m => Servant.Reflex.Client t m PokerAPI ()
myClient = client (Proxy :: Proxy PokerAPI)
                  (Proxy :: Proxy m)
                  (Proxy :: Proxy ())
                  (constDyn (BasePath "/"))

-- getAdd
--   :: forall t m. MonadWidget t m =>
--   (Dynamic t (Either T.Text Integer)
--                        -> Dynamic t (Either T.Text Integer)
--                        -> Event t ()
--                        -> m (Event t (ReqResult () Integer)))
-- getSub
--   :: forall t m. MonadWidget t m =>
--   (Dynamic t (Either T.Text Integer)
--   -> Dynamic t (Either T.Text Integer)
--   -> Event t ()
--   -> m (Event t (ReqResult () Integer)))

runQuery
  :: MonadWidget t m
  => Dynamic t (QParam [Char])
  -> Event t ()
  -> m (Event t
        (ReqResult () (Range Holding [PlayerActionValue])))
runQuery = let (runQuery' :<|> _ :<|> _) = myClient
           in runQuery'

loadHands
  :: forall t m. MonadWidget t m
  => Dynamic t (QParam [Char])
  -> Event t ()
  -> m (Event t (ReqResult () ()))
loadHands = let (_ :<|> loadHands' :<|> _) = myClient
            in loadHands'

getEcho
  :: forall t m. MonadWidget t m =>
  (Event t () -> m (Event t (ReqResult () T.Text)))
(_ :<|> _ :<|> getEcho) = myClient
      -- prerender_ (text "My Prerender") $ do
      --   headD <- fileHead
      --   headCM <- cmHead
      --   whenLoaded [headD, headCM] blank $ do
      --     tabDisplay "" "" $
      --       Map.fromList
      --       [ (1,
      --         ("Query Editor", do
      --                 el "div" $ do
      --                       dynCode <- fileBody
      --                       cmCode <- cmBody dynCode
      --                       dynText (fmap T.reverse cmCode)
      --         ))
      --       , (2,
      --         ("Range Display",
      --           prerender_ blank $ do
      --             el "div" $ do
      --                 let gs = constDyn initialRange
      --                 handClickE <- mkRangeDisplay gs
      --                 let tshow = T.pack . show
      --                 dynHand <- holdDyn (ShapedHand (Ace, Ace) Pair) handClickE -- fmap T.pack . fmap show . join $
      --                 dynText $ fmap tshow $ dynHand
      --                 return ()
      --           ))
      --       ]
      --     pure ()
      --   pure ()

      -- elAttr "img" ("src" =: static @"obelisk.jpg") blank
      -- el "div" $ do
      --   exampleConfig <- Obelisk.Configs.getConfig "common/example"
      --   case exampleConfig of
      --     Nothing -> text "No config file found in config/common/example"
      --     Just s -> text $ T.decodeUtf8 s