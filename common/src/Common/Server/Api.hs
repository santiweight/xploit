{-# Language DataKinds #-}
{-# Language PackageImports #-}
{-# Language TypeOperators #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language PartialTypeSignatures #-}
{-# Language TemplateHaskell #-}

module Common.Server.Api
        where

import Data.Map (Map)
import Servant.API
import Poker
import Poker.Range
import Common.DB.Instances ()
import Poker.History.Bovada.Model
import Poker.Game.Types
import Poker.Query.Eval.Internal
import Poker.Query.ActionIx
import Poker.Query.Eval.Types
import Money.Aeson
import Data.Aeson.TH

-- commonStuff :: String
-- commonStuff = "Here is a string defined in Common.Api"

-- Real API
type QueryAPI = "run"
              :> ReqBody '[JSON] NodeQueryRequest
              :> Get '[JSON] NodeQueryResponse

type Add = "add" :> (LoadHandHAPI :<|> AddHandFile)

type LoadHandHAPI = "load"
                  :> QueryParam "path" FilePath
                  :> Get '[JSON] ()

type AddHandFile = QueryParam "contents" String
                  :> Get '[JSON] ()

type Echo = "echo" :> Get '[JSON] ()

type PokerAPI = "api" :> (QueryAPI :<|> Add :<|> Echo)


data NodeQueryRequest = NodeQueryRequest
  { nodePath    :: [(Position, BetAction (IxRange (Amount "USD")))]
  , includeHero :: Bool
  , nodeFilter  :: BetAction (IxRange (Amount "USD"))
  }

data NodeQueryResponse = NodeQueryResponse
  { handsMatchedFilter :: [History (Amount "USD")]
  , holdingRange       :: Range Hand Double
  , shapedHandRange    :: Range ShapedHand Double
  }

deriveJSON defaultOptions ''NodeQueryRequest
deriveJSON defaultOptions ''NodeQueryResponse