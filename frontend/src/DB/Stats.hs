{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module DB.Stats where

import Reflex.Dom
import Handlers
import Control.Lens
import Servant.Common.Req (reqSuccess)
import BasicPrelude (tshow)

statsWidget :: MonadWidget t m => m ()
statsWidget = do
    stats <- (backendClient ^. statsClient) =<< getPostBuild
    widgetHold (text "not yet implemented") (text . tshow <$> mapMaybe reqSuccess stats)
    pure ()