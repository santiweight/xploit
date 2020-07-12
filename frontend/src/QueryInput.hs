{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ViewPatterns       #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ViewPatterns       #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}

module QueryInput where

import Control.Lens
import Reflex.Dom
import qualified Data.Text as T
import Data.Aeson (Value (String))
import qualified Reflex.CodeMirror as CM
import Reflex.Utils

cmHead :: forall t m. MonadWidget t m => m (Dynamic t Bool)
cmHead = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.css"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/theme/zenburn.css"
                     ]
    whenLoaded s1Ds blank $ do
        sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/mode/haskell/haskell.min.js"
                 ]
        return ()

cmBody :: MonadWidget t m => Dynamic t T.Text -> m (Dynamic t T.Text)
cmBody (updated -> bodyText) = do
    clickE <- button "goto line 3"
    let lineCharE = (Just $ CM.LineChar 3 1) <$ clickE
    textE <- CM.codemirror config bodyText lineCharE
    textD <- holdDyn startVal textE
    -- display textD
    pure textD
    where
        startVal :: T.Text
        startVal = "utgAct <- next\nacc r utgAct"
        config :: CM.Configuration
        config
            = def
            & CM.configuration_value ?~ startVal
            & CM.configuration_theme ?~ T.pack "zenburn"
            & CM.configuration_mode  ?~ (String $ T.pack "haskell")
