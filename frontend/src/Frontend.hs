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
{-# LANGUAGE NoImplicitPrelude #-}

module Frontend where

import Control.Lens hiding (ix)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as Map
import Reflex.Dom.Class
import Prelude hiding (mapM, mapM_, sequence, sequence_)
import Reflex
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Poker.Base
import Poker.Range
import Reflex.Dom.Core hiding (tabDisplay)
import Common.Server.Api
import Common.Route
import "servant-snap" Servant
import Servant.Reflex
import QueryInput
import RangeDisplay
import FileInput
import ActionIxSelector
import TabView

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

data TabChoice = RangeDisplay | HistoryDisplay
  deriving (Show, Read, Eq, Ord)

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
              text "hi"
              -- (codeD, shapedHandD) <- mainPane
              _ <- tabDisplay "" ""
                         (Map.fromList
                         [ ( RangeDisplay
                           , ("Range Display", Right <$> mainPane) )
                         , ( HistoryDisplay
                           , ("History Display", Left <$> blank) )
                         ])
              pure ()
            pure ()
          pure ()
  }

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
                          handD' <- holdDyn acesHand handClickE
                          fullRangeD' :: Dynamic t (Range Holding [PlayerActionValue])
                                      <- holdDyn (Range Map.empty) successResultE
                          pure (handD', fullRangeD')
              _ <- el "div" $
                dyn $ holdingTable handD (toHoldingFreqs <$> actionIxD <*> fullRangeD)
              runE <- el "div" $ button "run"
              resultE <- runQuery (QParamSome . T.unpack <$> codeD) runE
              let successResultE = mapMaybe reqSuccess resultE
            pure (codeD, handD)
  pure . bimap join join $ splitDynPure resD
  where
    toHoldingFreqs :: ActionIx -> Range Holding [PlayerActionValue] -> Range Holding Double
    toHoldingFreqs ix mainRange =
      mainRange
            & range . traverse %~ countFreqMatched (inIndex ix)
            & range . traverse %~ freqToDouble
    acesHand :: ShapedHand
    acesHand = ShapedHand (Ace, Ace) Pair
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
    freqToDouble (_, 0) = 0
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
loadHands
  :: forall t m. MonadWidget t m
  => Dynamic t (QParam [Char])
  -> Event t ()
  -> m (Event t (ReqResult () ()))
(runQuery :<|> loadHands) = myClient

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