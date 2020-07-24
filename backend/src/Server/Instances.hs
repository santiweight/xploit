{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server.Instances where

import Database.Persist.TH (derivePersistField)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Time.Calendar (Day (..))
import GHC.Generics
import Codec.Serialise
import Poker.Base

derivePersistField "GameType"

-- WARNiNG ORPHAN INSTANCE
deriving instance Generic LocalTime
deriving instance Generic Day
deriving instance Generic TimeOfDay
instance Serialise LocalTime
instance Serialise Day
instance Serialise TimeOfDay

instance Serialise Action
instance Serialise DealerAction
instance Serialise PlayerAction
instance Serialise IsHero
instance Serialise TableAction
instance Serialise TableActionValue
instance Serialise Card
instance Serialise Rank
instance Serialise Suit
instance Serialise BetAction
instance Serialise Position
instance Serialise Hand
instance Serialise Player
instance Serialise Holding
instance Serialise GameType
instance Serialise Network

