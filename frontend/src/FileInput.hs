{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE MonoLocalBinds       #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}

module FileInput where

import Control.Lens
import qualified Data.Text as T
-- import           "reflex-utils"   Reflex.Utils
-- import           "reflex-jexcel"  Reflex.JExcel
-- import           "reflex-fileapi" Reflex.FileAPI.FileAPI

import Reflex.Dom.Core

fileHead :: MonadWidget t m => m (Dynamic t Bool)
fileHead = constDyn True <$ text "fileHead"
--     s1Ds <- sequence [ script "https://bossanova.uk/jsuites/v2/jsuites.js"
--                      , css    "https://bossanova.uk/jsuites/v2/jsuites.css"
--                      ]
--     whenLoaded s1Ds blank $ do
--         s2Ds <- sequence [ script "https://bossanova.uk/jexcel/v3/jexcel.js"
--                          , css    "https://bossanova.uk/jexcel/v3/jexcel.css"
--                          ]
--         _ <- whenLoaded s2Ds blank blank
--         return ()

fileBody :: MonadWidget t m => m (Dynamic t T.Text)
fileBody = "fileBody" <$ text "fileBody" -- do undefined
--     -- button triggers everything
--     clickE  <- button "Load"

--     -- filereader is triggered by clickE
--     fileChunkTextE <- filereader "files[]" stepSize clickE

--     -- csvE :: Event t [[Text]]
--     let csvE = (map (T.splitOn ",") . T.lines) <$> fileChunkTextE
--     csvD <- holdDyn [] csvE

--     -- spreadsheet
--     let jexcelD = buildJExcel <$> csvD
--     _ <- jexcel (JExcelInput "excel1" jexcelD)

--     -- text display
--     display csvD

--     return $ T.unlines . fmap T.unlines <$> csvD
--     where
--         stepSize :: Int
--         stepSize = 10000000 -- bytes

--         headerToColumn :: T.Text -> JExcelColumn
--         headerToColumn name
--             = def
--             & jExcelColumn_title ?~ name

--         buildJExcel :: [[T.Text]] -> JExcel
--         buildJExcel [] = def
--         buildJExcel (headers:rows)
--             = def
--             & jExcel_columns    ?~ map headerToColumn headers
--             & jExcel_data       ?~ rows