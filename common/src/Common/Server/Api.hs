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
import Poker.Base
import Poker.Range
import Common.DB.Instances ()

-- commonStuff :: String
-- commonStuff = "Here is a string defined in Common.Api"

-- Real API
-- type QueryAPI = "run"
--               -- :> QueryParam "query" String
--               :> QueryParam "path" FilePath
--               -- :> ReqBody '[JSON] Filter
--               :> Get '[JSON] (Either (Either GameErrorBundle EvalErr) (Map String (Range Holding [BetAction])))

type QueryAPI = "run"
              :> QueryParam "query" String
              -- :> QueryParam "path" FilePath
              -- :> ReqBody '[JSON] Filter
              :> Get '[JSON] (Map String (Range Holding [BetAction]))

type Add = "add" :> (LoadHandHAPI :<|> AddHandFile)

type LoadHandHAPI = "load"
                  :> QueryParam "path" FilePath
                  :> Get '[JSON] ()

type AddHandFile = QueryParam "contents" String
                  :> Get '[JSON] ()

type Echo = "echo" :> Get '[JSON] ()

type PokerAPI = "api" :> (QueryAPI :<|> Add :<|> Echo)

