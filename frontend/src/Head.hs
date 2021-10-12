{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Head where

import           Common.Route                   ( FrontendRoute )
import           Obelisk.Frontend               ( ObeliskWidget )
import           Obelisk.Generated.Static
import           Obelisk.Route                  ( R )
import           Obelisk.Route.Frontend         ( RoutedT )
import           Reflex.Dom

head
  :: forall js t m
   . (ObeliskWidget js t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
head = do
  el "title" $ text "Xploit"
  jQueryJs
  jQueryUiJs
  noUiSliderCss
  noUiSliderJs
  xploitCss
  pureCss
 where
  cssLink uri = cssLinkAttrs uri mempty
  cssLinkAttrs uri attrs =
    elAttr "link" ("href" =: uri <> "rel" =: "stylesheet" <> attrs) blank
  script uri = elAttr "script" ("src" =: uri) blank
  jQueryJs   = script "https://code.jquery.com/jquery-1.12.4.js"
  jQueryUiJs = script "https://code.jquery.com/ui/1.12.1/jquery-ui.js"
  noUiSliderCss =
    cssLink "//cdn.bootcss.com/noUiSlider/14.6.2/nouislider.min.css"
  noUiSliderJs = script "//cdn.bootcss.com/noUiSlider/14.6.2/nouislider.js"
  xploitCss    = cssLink $ static @"css/main.css"
  pureCss      = cssLinkAttrs
    "https://unpkg.com/purecss@2.0.5/build/pure-min.css"
    (  "integrity"
    =: "sha384-LTIDeidl25h2dPxrB2Ekgc9c7sEC3CWGM6HeFmuDNUjX76Ert4Z4IY714dhZHPLd"
    <> "crossorigin"
    =: "anonymous"
    )
