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
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend where

import Control.Lens hiding (ix)
import Data.Witherable (catMaybes)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Reflex.Dom.Class
import Reflex.Class
import Prelude hiding (mapM, mapM_, sequence, sequence_)
import Reflex
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Poker.Base
import Poker.Range
import Reflex.Dom hiding (tabDisplay)

import Data.Maybe (fromMaybe)
import Network.URI.Encode

import Data.Proxy
import Common.Server.Api
import Common.Route
import Servant.API
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
  , _frontend_body = body
  }

body :: forall js t m route. ObeliskWidget js t route m => RoutedT t route m ()
-- body :: _
body = do
        prerender_ blank $ do
          echoE <- button "echo"
          resE <- doEcho echoE
          resD <- holdDyn ("") (fromMaybe ("") <$> resE)
          dynText resD
          -- filesD :: Dynamic t [File] <- el "div" $ fileInputElement
          -- let filesE :: Event t [File] = updated $ filesD
          -- let textEsE :: Event t
          --                  [RoutedT t route (Reflex.Dom.Client m) (Event t Text)]
          --             = filesE <&> fmap dataURLFileReader
          -- textEsD <- fmap sequence <$> holdDyn [] textEsE
          -- _ <- dyn textEsD
          -- textEsD :: Dynamic t [_ (Event t Text)] <- holdDyn [] textEsE
          -- let mkRequest :: (Prerender js t m, MonadWidget t m) => Event t Text -> m (Event t (ReqResult () ()))
          -- let mkRequest contentEvent = do
          --               content :: Dynamic t (QParam [Char])
          --                       <- fmap (QParamSome . T.unpack) <$> holdDyn  "" contentEvent
          --               loadHands content (void contentEvent)

          -- let requests :: Dynamic t (RoutedT t route (Reflex.Dom.Client m) [Event t (ReqResult () ())])
          --              = textEsD <&> \fileContentsEsM -> (sequence $ (fileContentsEsM <&> (>>= mkRequest)))
          -- -- -- requests
          -- _ <- dyn requests :: RoutedT t route (Reflex.Dom.Client m) (Event t [Event t (ReqResult () ())])
          -- dyn undefined :: RoutedT t route (Reflex.Dom.Client m) (Event t [Event t (ReqResult () ())])
          pure ()
        --       .
        el "div" $ do
          prerender_ blank $ do
            headD <- fileHead
            headCM <- cmHead
            void . whenLoaded [headD, headCM] blank $ do
              -- (codeD, shapedHandD) <- mainPane
              void $ tabDisplay "" ""
                         (Map.fromList
                         [ ( RangeDisplay
                           , ("Range Display", Right <$> mainPane) )
                         , ( HistoryDisplay
                           , ("History Display", Left <$> blank) )
                         ])

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- fileInputElement :: DomBuilder t m => m (Dynamic t [File])
-- fileInputElement = do
--   ie <- inputElement $ def
--     & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
--       ("type" =: "file"
--       <> "id" =: "filepicker"
--       <> "name" =: "fileList"
--       <> "webkitdirectory" =: ""
--       <> "multiple" =: "")
--   return (_inputElement_files ie)

-- dataURLFileReader
--   :: ( DomBuilder t m
--      , TriggerEvent t m
--      , PerformEvent t m
--      , Prerender js t m
--      )
--   => File -> m (Event t Text)
-- dataURLFileReader request = fmap switchDyn . prerender (return never) $ do
--   fileReader :: FileReader <- liftJSM newFileReader
--   readAsText fileReader (Just request) (Nothing :: Maybe Text)
--   e <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
--     v <- getResult fileReader
--     (fromJSVal <=< toJSVal) v
--   return (fmapMaybe id e)

mainPane
  :: forall js t m. (MonadWidget t m, Prerender js t m)
  => m (Dynamic t T.Text, Dynamic t ShapedHand)
mainPane = do
  resD <- prerender ((constDyn "", constDyn acesHand) <$ text "My Prerender") $
          do
            rec
              actionIxD <- join <$> prerender (constDyn AnyIx <$ blank) actionIxSelector
              let getMainRange :: Dynamic t (Range Holding [BetAction]
                                    -> Range ShapedHand Double)
                  getMainRange = (getDisplayRange <$> actionIxD)
              fileContentsDyn <- fileBody
              codeD <- el "div" $ (cmBody fileContentsDyn)
              (handD, fullRangesD) <- el "div" $ do
                          handClickE <- mkRangeDisplay $ getMainRange <*> fullRangesD
                          handD' <- holdDyn acesHand handClickE
                          fullRangesD' :: Dynamic t (Range Holding [BetAction])
                                      <- fmap (fromMaybe (Range Map.empty).fmap snd.safeHead.Map.assocs) <$> holdDyn (Map.empty) successResultE
                          pure (handD', fullRangesD')
              _ <- el "div" $
                dyn $ holdingTable handD (toHoldingFreqs <$> actionIxD <*> fullRangesD)
              runE <- el "div" $ button "run"
              response <- runQuery codeD runE
              -- dynText =<< holdDyn "no result" (fromMaybe "empty response" <$> traceEventWith show response)
              -- resultE <- runQuery (QParamSome . T.unpack <$> codeD) runE
              -- let successResultE = mapMaybe reqSuccess $ resultE
              let successResultE :: Event t _ = traceEventWith (const "response got") $ catMaybes response
              -- let successResultE = mapMaybe getSuccess . mapMaybe reqSuccess $ resultE
              -- let getSuccess = \case
              --             Right (res) -> Just res
              --             _ -> Nothing
            pure (codeD, handD)
  pure . bimap join join $ splitDynPure resD
  where
    toHoldingFreqs :: ActionIx -> Range Holding [BetAction] -> Range Holding Double
    toHoldingFreqs ix mainRange =
      mainRange
            & range . traverse %~ countFreqMatched (inIndex ix)
            & range . traverse %~ freqToDouble
    acesHand :: ShapedHand
    acesHand = ShapedHand (Ace, Ace) Pair
    groupByShape
      :: Range Holding [BetAction]
      -> Range ShapedHand [BetAction]
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
      -> Range Holding [BetAction]
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
                  (constDyn (BaseFullUrl Http "localhost" 8000 ""))

-- -- runQuery
-- --   :: MonadWidget t m
-- --   => Dynamic t (QParam [Char])
-- --   -> Event t ()
-- --   -> m (Event t
-- --         (ReqResult () (Either (Either GameErrorBundle EvalErr) (Map.Map String (Range Holding [BetAction])))))
-- runQuery
--   :: MonadWidget t m
--   => Dynamic t (QParam [Char])
--   -> Event t ()
--   -> m (Event t
--         (ReqResult () (Map.Map String (Range Holding [BetAction]))))
-- loadHands
--   :: forall t m. MonadWidget t m
--   => Dynamic t (QParam [Char])
--   -> Event t ()
--   -> m (Event t (ReqResult () ()))
-- (runQuery :<|> (loadHands :<|> addHandFileContents) :<|> doEcho) = myClient

      -- elAttr "img" ("src" =: static @"obelisk.jpg") blank
      -- el "div" $ do
      --   exampleConfig <- Obelisk.Configs.getConfig "common/example"
      --   case exampleConfig of
      --     Nothing -> text "No config file found in config/common/example"
      --     Just s -> text $ T.decodeUtf8 s

runUrl query = "http://localhost:8000/api/run?query=" <> query

echoUrl = "http://localhost:8000/api/echo"

doEcho :: MonadWidget t m
  => Event t ()
  -> m (Event t (Maybe Text))
doEcho echoE = do
  reponses <- performRequestAsync $ XhrRequest "GET" echoUrl def <$ echoE
  return $ view (xhrResponse_responseText) <$> reponses

runQuery
  :: MonadWidget t m
  => Dynamic t Text
  -> Event t ()
  -> m (Event t (Maybe (Map.Map String (Range Holding [BetAction]))))
runQuery queryD event = do
  -- reponses <- performRequestAsync $ XhrRequest "GET" echoUrl def <$ event
  -- return $ view (xhrResponse_responseText) <$> reponses
  responses <- performRequestAsync $ traceEventWith show $ toRequest . encodeText <$> tagPromptlyDyn queryD event
  return . fmap (Reflex.Dom.decodeText =<<) $ view xhrResponse_responseText <$> responses
  where toRequest query = XhrRequest "GET" (runUrl query) def