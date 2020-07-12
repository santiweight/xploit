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

module TabView where

import Control.Lens
import Reflex.Dynamic
import Reflex.Dom.Core
import Data.Text (Text)
import Control.Monad
import Control.Monad.Fix
import qualified Data.Map as Map
import Data.Map (Map)

tabDisplay :: forall t m k a. (MonadFix m, MonadWidget t m, Show k, Ord k)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Map k (Text, m a) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m (Dynamic t (Map k (Event t a)))
tabDisplay ulClass activeClass tabItems = do
  rec dCurrentTab :: Dynamic t (Maybe k) <- holdDyn Nothing (updated dTabClicks)
      dTabClicks :: Dynamic t (Maybe k) <- elAttr "ul" (Map.singleton "class" ulClass) $ do
        tabClicksList :: [Event t k] <- (liftM Map.elems) $ imapM (\k (s,_) -> headerBarLink s k $ (== (Just k)) <$> dCurrentTab) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        holdDyn Nothing $ fmap Just eTabClicks :: m (Dynamic t (Maybe k))
  divClass "" $ do
    let dTabs :: Dynamic t (Map k (Text, m a)) = constDyn tabItems
    mapOfEventsD <- listWithKey dTabs (\k dTab -> do
        let dAttrs :: Dynamic t (Map Text Text) = dCurrentTab <&> (\sel ->
              let t1 = listToMaybe $ Map.keys tabItems
              in if sel == Just k || (sel == Nothing && t1 == Just k) then (Map.empty :: Map Text Text) else Map.singleton "style" "display:none;")
        elDynAttr "div" dAttrs . dyn $ snd <$> dTab)
    pure mapOfEventsD
  where
    listToMaybe (x:_) = Just x
    listToMaybe _ = Nothing
    headerBarLink :: (Reflex t, MonadWidget t m, Ord k) => Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k dBool = do
      let dAttributes :: Dynamic t (Map Text Text) = dBool <&> (\b -> if b then Map.singleton "class" activeClass else Map.empty)
      elDynAttr "li" dAttributes $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)