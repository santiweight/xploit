{-# Language DataKinds #-}
{-# Language PackageImports #-}
{-# Language TypeOperators #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language PartialTypeSignatures #-}

module Common.Server.Api
       ( QueryAPI
       , Add
       , LoadHandHAPI
       , AddHandFile
       , Echo
       , PokerAPI
      --  , module Common.DB.Instances
       ) where

import Data.Map (Map)
import Servant.API
import Poker
import Poker.Range
import Common.DB.Instances ()
import Poker.Game.Types
import Poker.Query.Eval.Internal
import Poker.Query.ActionIx
import Poker.Query.Eval.Types

-- commonStuff :: String
-- commonStuff = "Here is a string defined in Common.Api"

-- Real API
type QueryAPI = "run"
              :> ReqBody '[JSON] [(Position, BetAction (IxRange (Amount "USD")))]
              :> Get '[JSON] (Map String (Range Hand [BetAction (Amount "USD")]))

type Add = "add" :> (LoadHandHAPI :<|> AddHandFile)

type LoadHandHAPI = "load"
                  :> QueryParam "path" FilePath
                  :> Get '[JSON] ()

type AddHandFile = QueryParam "contents" String
                  :> Get '[JSON] ()

type Echo = "echo" :> Get '[JSON] ()

type PokerAPI = "api" :> (QueryAPI :<|> Add :<|> Echo)

