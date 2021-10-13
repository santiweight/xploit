{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import           Common.Server.Api
import           Control.Lens                   ( makeLenses
                                                )
import           Data.Text                      ( Text )
import           Reflex.Dom
import           Servant.Reflex                 ( QParam
                                                , ReqResult, client
                                                , BaseUrl(BasePath)

                                                )
import Servant.API
import Data.Proxy (Proxy (Proxy))

-- loadHandClient :: forall t m. MonadWidget t m => AddHandClient t m
-- loadHandClient = do
--   let (_loadDir :<|> _addHandContents) = client
--         (Proxy :: Proxy Add)
--         (Proxy :: Proxy m)
--         (Proxy :: Proxy ())
--         (constDyn (BasePath "/"))
--   AddHandClient undefined _loadDir

loadHandClient :: forall t m. MonadWidget t m => AddHandClient t m
loadHandClient = do
  let _loadDir = client
        (Proxy :: Proxy LoadHandHAPI)
        (Proxy :: Proxy m)
        (Proxy :: Proxy ())
        (constDyn (BasePath "/"))
  AddHandClient _loadDir

backendClient :: forall t m . MonadWidget t m => BackendClient t m
backendClient = do
  let (_queryApi :<|> _ :<|> _echo) = client (Proxy :: Proxy PokerAPI)
                                             (Proxy :: Proxy m)
                                             (Proxy :: Proxy ())
                                             (constDyn (BasePath "/"))
  MyClient _queryApi loadHandClient _echo

data AddHandClient t m = AddHandClient
  -- { addFilesApi
  --     :: Dynamic t (QParam [FilePath])
  --     -> Event t ()
  --     -> m (Event t (ReqResult () ()))
  { _addDirApi
      :: Dynamic t (QParam FilePath)
      -> Event t ()
      -> m (Event t (ReqResult () ()))
  }

data BackendClient t m = MyClient
  { _queryApi
      :: Dynamic t (Either Text NodeQueryRequest)
      -> Event t ()
      -> m (Event t (ReqResult () NodeQueryResponse))
  , _addClient :: AddHandClient t m
  , _echo
      :: Event t ()
      -> m (Event t (ReqResult () ()))
  }

makeLenses ''BackendClient