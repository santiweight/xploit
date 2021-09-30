{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language PackageImports #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
{-# language DerivingVia #-}
{-# language StandaloneDeriving #-}
{-# language DeriveGeneric #-}

module Common.DB.Instances where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), ToJSONKey (..), FromJSONKey (..), defaultJSONKeyOptions, genericFromJSONKey, genericToJSONKey)
import Poker
import Poker.Query.ActionIx
-- import Poker.History.Model
-- import Poker.History.Types
import Poker.Game.Types
-- import Poker.Game
import Poker.Query.Eval.Types
import Poker.Query.Eval.AST.Types
import Poker.Range
import GHC.Generics
import Data.Map.Strict
import Money
import GHC.TypeLits

instance FromJSONKey Position where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSONKey Position where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance ToJSONKey ShapedHand
instance FromJSONKey ShapedHand
instance ToJSONKey Hand
instance FromJSONKey Hand
instance ToJSONKey Seat
instance FromJSONKey Seat

deriving instance Generic Position
deriving via (Map a b) instance (FromJSONKey a, Ord a, FromJSON b) => FromJSON (Range a b)
deriving via (Map a b) instance (ToJSONKey a, Ord a, ToJSON b) => ToJSON (Range a b)
instance (FromJSON (Discrete' b (CurrencyScale b)), KnownSymbol b, GoodScale (CurrencyScale b)) => FromJSON (Amount b) where
  parseJSON = fmap Amount . parseJSON
instance (ToJSON (Discrete' b (CurrencyScale b))) => ToJSON (Amount b) where
  toJSON (Amount amt) = toJSON amt
-- deriving instance Generic (SNetwork net)
-- deriveJSON defaultOptions ''GameErrorBundle
-- deriveJSON defaultOptions ''GameError
-- deriveJSON defaultOptions ''GameState
-- deriveJSON defaultOptions ''ActionFaced
-- deriveJSON defaultOptions ''EvalErr
-- deriveJSON defaultOptions ''Var
-- deriveJSON defaultOptions ''BetType
deriveJSON defaultOptions ''Board
-- deriveJSON defaultOptions ''Header
deriveJSON defaultOptions ''Seat
-- deriveJSON defaultOptions ''History
deriveJSON defaultOptions ''Stake
-- deriveJSON defaultOptions ''BetAction
deriveJSON defaultOptions ''GameError
deriveJSON defaultOptions ''GameErrorBundle
deriveJSON defaultOptions ''GameState
deriveJSON defaultOptions ''Pot
deriveJSON defaultOptions ''ActionFaced
deriveJSON defaultOptions ''BetType
deriveJSON defaultOptions ''Stack
deriveJSON defaultOptions ''PlayerAction
deriveJSON defaultOptions ''PostAction
deriveJSON defaultOptions ''PostActionValue
deriveJSON defaultOptions ''Var
deriveJSON defaultOptions ''EvalErr
deriveJSON defaultOptions ''BetAction
-- deriveJSON defaultOptions ''TableAction
-- deriveJSON defaultOptions ''TableActionValue
deriveJSON defaultOptions ''DealerAction
deriveJSON defaultOptions ''IxRange
deriveJSON defaultOptions ''ActionIx
deriveJSON defaultOptions ''Card
deriveJSON defaultOptions ''Rank
deriveJSON defaultOptions ''Suit
deriveJSON defaultOptions ''Position
-- deriveJSON defaultOptions ''Network
deriveJSON defaultOptions ''Player
-- deriveJSON defaultOptions ''Hand
-- deriveJSON defaultOptions ''GameType
deriveJSON defaultOptions ''IsHero
deriveJSON defaultOptions ''Action
deriveJSON defaultOptions ''Hand
-- deriveJSON defaultOptions ''Shape
deriveJSON defaultOptions ''ShapedHand

