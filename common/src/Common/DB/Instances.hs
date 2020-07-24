{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language PackageImports #-}
{-# language TemplateHaskell #-}
{-# language StandaloneDeriving #-}
{-# language DeriveGeneric #-}

module Common.DB.Instances where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Aeson (ToJSONKey (..), FromJSONKey (..), defaultJSONKeyOptions, genericFromJSONKey, genericToJSONKey)
import Poker.Base

instance FromJSONKey Position where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSONKey Position where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance ToJSONKey ShapedHand
instance FromJSONKey ShapedHand
instance ToJSONKey Holding
instance FromJSONKey Holding

-- deriveJSON defaultOptions ''GameErrorBundle
-- deriveJSON defaultOptions ''GameError
-- deriveJSON defaultOptions ''GameState
-- deriveJSON defaultOptions ''ActionFaced
-- deriveJSON defaultOptions ''EvalErr
-- deriveJSON defaultOptions ''Var
-- deriveJSON defaultOptions ''BetType
deriveJSON defaultOptions ''Board
deriveJSON defaultOptions ''Stake
deriveJSON defaultOptions ''PlayerAction
deriveJSON defaultOptions ''BetAction
deriveJSON defaultOptions ''TableAction
deriveJSON defaultOptions ''TableActionValue
deriveJSON defaultOptions ''DealerAction
deriveJSON defaultOptions ''IxRange
deriveJSON defaultOptions ''ActionIx
deriveJSON defaultOptions ''Card
deriveJSON defaultOptions ''Rank
deriveJSON defaultOptions ''Suit
deriveJSON defaultOptions ''Position
deriveJSON defaultOptions ''Network
deriveJSON defaultOptions ''Player
deriveJSON defaultOptions ''Holding
deriveJSON defaultOptions ''GameType
deriveJSON defaultOptions ''IsHero
deriveJSON defaultOptions ''Action
deriveJSON defaultOptions ''Hand
deriveJSON defaultOptions ''Shape
deriveJSON defaultOptions ''ShapedHand

