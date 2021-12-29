{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Server.Api where

import Common.DB.Instances ()
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Map (Map)
import Data.Some
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits
import Money
import Money.Aeson
import Poker
import Poker.Game.Types
import Poker.History.Bovada.Model
import Poker.History.Types (Curr (EUR, GBP, USD), SomeBetSize, SomeCurr)
import Poker.Query.ActionIx
import Poker.Query.Eval.Internal
import Poker.Query.Eval.Types
import Poker.Range
import Servant.API

-- commonStuff :: String
-- commonStuff = "Here is a string defined in Common.Api"

-- Real API
type QueryAPI =
  "run"
    :> ReqBody '[JSON] SomeNodeQueryRequest
    :> Post '[JSON] NodeQueryResponse

-- type Add = "add" :> (LoadHandHAPI :<|> AddHandFile)

type LoadHandHAPI =
  "load"
    :> QueryParam "path" FilePath
    :> Get '[JSON] ()

data ReviewHistory = ReviewHistory
  { holes :: Maybe (Map Position Hand),
    stacks :: Map Position (Stack (Amount "USD")),
    acts :: [ReviewAction]
  }
  deriving (Show, Eq)

data ReviewAction = ReviewAction
  { act :: Poker.Game.Types.Action (Amount "USD"),
    comment :: Text
  }
  deriving (Show, Eq)

type HandReview =
  "review"
    :> ( ("submit" :> ReqBody '[JSON] ReviewHistory :> Post '[JSON] ())
           :<|> ( "range"
                    :> ReqBody '[JSON] [Poker.Game.Types.Action (Amount "USD")]
                    :> Post '[JSON] (Map Int (Range Hand Double, Range ShapedHand Double))
                )
       )

type StatsApi =
  "stats" :> Get '[JSON] (Map String String)

type AddHandFile =
  QueryParam "contents" String
    :> Get '[JSON] ()

type Echo = "echo" :> Get '[JSON] ()

-- type PokerAPI = "api" :> (QueryAPI :<|> Add :<|> Echo)
type PokerAPI = "api" :> (QueryAPI :<|> LoadHandHAPI :<|> Echo :<|> HandReview :<|> StatsApi)

data SomeNodeQueryRequest where
  SomeNodeQueryRequest ::
    ( FromJSON (Discrete' b (CurrencyScale b)),
      KnownSymbol b,
      GoodScale (CurrencyScale b)
    ) =>
    Curr b ->
    NodeQueryRequest b ->
    SomeNodeQueryRequest

data NodeQueryRequest b = NodeQueryRequest
  { nodePath :: [(Position, BetAction (IxRange (Amount b)))],
    includeHero :: Bool,
    nodeExpectedPos :: Position,
    nodeFilter :: BetAction (IxRange (Amount b)),
    nodeNormalisation :: Normalisation
  }
  deriving (Show, Generic)

data NodeQueryResponse = NodeQueryResponse
  { handsMatchedFilter :: [History (Amount "USD")],
    holdingRange :: Range Hand Double,
    shapedHandRange :: Range ShapedHand Double
  }
  deriving (Show, Generic)

data Normalisation = NormToBB | NoNorm
  deriving (Show, Generic)

instance
  ( FromJSON (Discrete' b (CurrencyScale b)),
    KnownSymbol b,
    GoodScale (CurrencyScale b)
  ) =>
  FromJSON (NodeQueryRequest b)
  where
  parseJSON = genericParseJSON defaultOptions

instance
  ( ToJSON (Discrete' b (CurrencyScale b)),
    KnownSymbol b,
    GoodScale (CurrencyScale b)
  ) =>
  ToJSON (NodeQueryRequest b)
  where
  toJSON = genericToJSON defaultOptions

instance FromJSON SomeNodeQueryRequest where
  parseJSON = withObject "SomeNodeQueryRequest" $ \j -> do
    curr :: Some Curr <- j .: "currency"
    -- req :: NodeQueryRequest b <- j .: "request"
    case curr of
      Some cu -> case cu of
        USD -> SomeNodeQueryRequest USD <$> (j .: "request")
        EUR -> SomeNodeQueryRequest EUR <$> (j .: "request")
        GBP -> SomeNodeQueryRequest GBP <$> (j .: "request")

instance ToJSON SomeNodeQueryRequest where
  toJSON foo = case foo of
    SomeNodeQueryRequest cu nqr -> object ["currency" Data.Aeson..= toJSON cu, "request" Data.Aeson..= toJSON nqr]

deriveJSONGADT ''Curr

-- deriveJSON defaultOptions ''NodeQueryRequest
deriveJSON defaultOptions ''Normalisation
deriveJSON defaultOptions ''NodeQueryResponse
deriveJSON defaultOptions ''ReviewHistory
deriveJSON defaultOptions ''ReviewAction
