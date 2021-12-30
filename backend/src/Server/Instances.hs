{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server.Instances where

import Codec.Serialise
import Common.Server.Api
import Data.Time.Calendar (Day (..))
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Database.Persist.TH (derivePersistField)
import GHC.Generics
import GHC.TypeLits
import Money
import Money.Serialise ()
import Poker
import qualified Poker.Game.Types
import Poker.History.Bovada.Model

derivePersistField "GameType"

-- WARNiNG ORPHAN INSTANCE
deriving instance Generic LocalTime

deriving instance Generic Day

deriving instance Generic TimeOfDay

deriving instance Generic b => Generic (Action b)

deriving instance Generic b => Generic (PlayerAction b)

deriving instance Generic b => Generic (TableAction b)

deriving instance Generic b => Generic (TableActionValue b)

deriving instance Generic b => Generic (BetAction b)

deriving instance Generic DealerAction

deriving instance Generic Hand

deriving instance Generic IsHero

deriving instance Generic Card

deriving instance Generic Rank

deriving instance Generic Suit

instance Serialise LocalTime

instance Serialise Day

instance Serialise TimeOfDay

instance (Generic b, Serialise b) => Serialise (Action b)

instance Serialise DealerAction

instance (Generic b, Serialise b) => Serialise (PlayerAction b)

instance Serialise Position

instance Serialise IsHero

instance (Generic b, Serialise b) => Serialise (TableAction b)

instance (Generic b, Serialise b) => Serialise (TableActionValue b)

instance Serialise Header

deriving instance Generic (Stake b)

deriving instance Generic Seat

instance (KnownSymbol b, GoodScale (CurrencyScale b)) => Generic (Amount b) where
  type Rep (Amount b) = Rep (Discrete' b (CurrencyScale b))
  to = unsafeMkAmount . to
  from (Amount amt) = from amt

instance (KnownSymbol b, GoodScale (CurrencyScale b)) => Serialise (Amount b) where
  encode (Amount amt) = encode $ toSomeDiscrete amt
  decode = do
    someDiscrete <- decode
    case fromSomeDiscrete someDiscrete of
      Nothing -> fail "Bad discrete type"
      Just disc -> pure $ unsafeMkAmount disc

instance Serialise Seat

deriving instance Generic (Stack b)

instance (Generic b, Serialise b) => Serialise (Stack b)

instance (Generic b, Serialise b) => Serialise (Stake b)

instance (Generic b, Serialise b) => Serialise (History b)

instance Serialise Card

instance Serialise Rank

instance Serialise Suit

instance (Generic b, Serialise b) => Serialise (BetAction b)

instance Serialise Hand

instance (Generic b, Serialise b) => Serialise (Player b)

instance Serialise GameType

instance Serialise Network

deriving instance Generic (Poker.Game.Types.Action b)

instance (Serialise b, Generic b) => Serialise (Poker.Game.Types.Action b)

deriving instance Generic (Poker.Game.Types.PlayerAction b)

instance (Serialise b, Generic b) => Serialise (Poker.Game.Types.PlayerAction b)

deriving instance Generic (Poker.Game.Types.PostAction b)

instance (Serialise b, Generic b) => Serialise (Poker.Game.Types.PostAction b)

deriving instance Generic (Poker.Game.Types.PostActionValue b)

instance (Serialise b, Generic b) => Serialise (Poker.Game.Types.PostActionValue b)

deriving instance Generic Poker.Game.Types.DealerAction

instance Serialise Poker.Game.Types.DealerAction

deriving instance Generic ReviewAction

instance Serialise ReviewAction

deriving instance Generic ReviewHistory

instance Serialise ReviewHistory
