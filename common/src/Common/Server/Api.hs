{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Common.Server.Api where

import Common.DB.Instances ()
import Data.Aeson.TH
import Data.Map (Map)
import Data.Text (Text)
import Money.Aeson
import Poker
import Poker.Game.Types
import Poker.History.Bovada.Model
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
    :> ReqBody '[JSON] NodeQueryRequest
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

type AddHandFile =
  QueryParam "contents" String
    :> Get '[JSON] ()

type Echo = "echo" :> Get '[JSON] ()

-- type PokerAPI = "api" :> (QueryAPI :<|> Add :<|> Echo)
type PokerAPI = "api" :> (QueryAPI :<|> LoadHandHAPI :<|> Echo :<|> HandReview)

data NodeQueryRequest = NodeQueryRequest
  { nodePath :: [(Position, BetAction (IxRange (Amount "USD")))],
    includeHero :: Bool,
    nodeExpectedPos :: Position,
    nodeFilter :: BetAction (IxRange (Amount "USD"))
  }
  deriving (Show)

data NodeQueryResponse = NodeQueryResponse
  { handsMatchedFilter :: [History (Amount "USD")],
    holdingRange :: Range Hand Double,
    shapedHandRange :: Range ShapedHand Double
  }
  deriving (Show)

deriveJSON defaultOptions ''NodeQueryRequest
deriveJSON defaultOptions ''NodeQueryResponse
deriveJSON defaultOptions ''ReviewHistory
deriveJSON defaultOptions ''ReviewAction