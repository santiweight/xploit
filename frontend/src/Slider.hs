{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Slider where

import Reflex.Dom
import qualified BasicPrelude as P
import qualified Data.Text as T
import Text.Megaparsec
import Data.Text (Text)
import Control.Lens (Identity(Identity), Bifunctor (bimap))
import JSDOM.Types (IsElement, JSM, Element (unElement), toElement)
import Language.Javascript.JSaddle (nextAnimationFrame, jsg, (!), (#), function, fun, valToText, eval, liftJSM)
import Control.Monad.IO.Class (liftIO)
import Text.Megaparsec.Char (char)

data SingleSliderTy = AboveSlider | BelowSlider | ExactlySlider

doubleSlider
  :: forall t m js
   . (DomBuilder t m, Prerender js t m)
  => (Int, Int)
  -> m (Event t (Int, Int))
doubleSlider (lo, hi) = fmap switchDyn $ prerender (never <$ blank) $ do
  (sliderEl   , ()) <- elAttr' "div" ("class" =: "raiseSlider") blank
  (sliderValsE, sliderValsCB :: (Int, Int) -> IO ()) <- newTriggerEvent
  liftJSM $ sliderInner (_element_raw sliderEl)
                        opts
                        (lo, hi)
                        readBetweenSliderValP
                        sliderValsCB
  pure sliderValsE
 where
  opts =
    "var opts = {connect: true, start: ["
    <> P.tshow lo
    <> ","
    <> P.tshow hi
    <> "], range: {'min':"
    <> P.tshow lo
    <> ", 'max':"
    <> P.tshow hi
    <> "}}; opts" :: T.Text
  readBetweenSliderValP :: ParsecT String Text Identity (Int, Int)
  readBetweenSliderValP = bimap (truncate. (*100)) (truncate. (*100)) <$> P.liftA2 (,) (float <* char ',') float

float :: ParsecT String Text Identity Double
float = error "not implemented"

singleSlider
  :: forall t m js
   . (DomBuilder t m, Prerender js t m)
  => SingleSliderTy
  -> (Int, Int)
  -> m (Event t Int)
singleSlider singleSliderTy (lo, hi) =
  fmap switchDyn $ prerender (never <$ blank) $ do
    (sliderEl, ()) <- elAttr' "div" ("class" =: "raiseSlider") blank
    (sliderValsE, sliderValsCB :: Int -> IO ()) <- newTriggerEvent
    liftJSM
      $ sliderInner (_element_raw sliderEl) opts initVal (truncate . (*100) <$> float) sliderValsCB
    -- getPostBuild >>= \ev -> (performEvent_ $ ev <&> \_ -> liftIO (sliderValsCB (P.tshow amtRange)))
    pure sliderValsE
 where
  initVal = truncate $ fromIntegral @_ @Double lo + fromIntegral hi / 2
  opts =
    "var opts = {"
    <> case singleSliderTy of
         AboveSlider   -> "connect: [false, true],"
         BelowSlider   -> "connect: [true, false],"
         ExactlySlider -> ""
    <> "start: ["
    <> P.tshow initVal
    <> "], range: {'min':"
    <> P.tshow lo
    <> ", 'max':"
    <> P.tshow hi
    <> "}}; opts" :: T.Text

sliderInner
  :: (IsElement el)
  => el
  -> Text
  -> a
  -> ParsecT String Text Identity a
  -> (a -> IO ())
  -> JSM ()
sliderInner el_ opts initVal pSliderVal updateCallBack = do
  let js_el = unElement . toElement $ el_
  nextAnimationFrame $ \_ -> do
    _ <- jsg ("noUiSlider" :: Text) # ("create" :: Text) $ (js_el, eval opts)
    updateF <- function $ fun $ \_ _ [sliderValJS, _, _, _, _, _] -> do
      sliderVal <- valToText sliderValJS
      liftIO $ updateCallBack $ readSliderValWith pSliderVal sliderVal
    _ <-
      (js_el ! ("noUiSlider" :: Text))
      # ("on" :: Text)
      $ ("update" :: T.Text, updateF)
    liftIO $ updateCallBack initVal
  pure ()

readSliderValWith :: forall a . ParsecT String Text Identity a -> Text -> a
readSliderValWith p str = P.fromMaybe undefined (parseMaybe p str :: Maybe a)