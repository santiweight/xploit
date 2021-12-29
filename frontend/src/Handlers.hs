{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Handlers where

import Common.Server.Api
import Common.Server.Api (ReviewHistory)
import Control.Lens
  ( makeLenses,
  )
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Poker
import qualified Poker.Game.Types
import Reflex.Dom
import Servant.API
import Servant.Reflex
  ( BaseUrl (BasePath),
    QParam,
    ReqResult,
    client,
  )

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
  let _loadDir =
        client
          (Proxy :: Proxy LoadHandHAPI)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BasePath "/"))
  AddHandClient _loadDir

backendClient :: forall t m. MonadWidget t m => BackendClient t m
backendClient = do
  let (_queryApi :<|> _ :<|> _echo :<|> (reviewSubmitApi :<|> reviewRangesApi) :<|> statsClient)  =
        client
          (Proxy :: Proxy PokerAPI)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BasePath "/"))
  BackendClient _queryApi loadHandClient (ReviewClient reviewSubmitApi reviewRangesApi) _echo statsClient

data ReviewClient t m = ReviewClient
  { postForReview ::
      Dynamic t (Either Text ReviewHistory) ->
      Event t () ->
      m (Event t (ReqResult () ())),
    reviewRanges ::
      Dynamic t (Either Text [Poker.Game.Types.Action (Amount "USD")]) ->
      Event t () ->
      m (Event t (ReqResult () (Map Int (Range Hand Double, Range ShapedHand Double))))
  }

data AddHandClient t m = AddHandClient
  -- { addFilesApi
  --     :: Dynamic t (QParam [FilePath])
  --     -> Event t ()
  --     -> m (Event t (ReqResult () ()))
  { _addDirApi ::
      Dynamic t (QParam FilePath) ->
      Event t () ->
      m (Event t (ReqResult () ()))
  }

data BackendClient t m = BackendClient
  { _queryApi ::
      Dynamic t (Either Text SomeNodeQueryRequest) ->
      Event t () ->
      m (Event t (ReqResult () NodeQueryResponse)),
    _addClient :: AddHandClient t m,
    _reviewClient :: ReviewClient t m,
    _echo ::
      Event t () ->
      m (Event t (ReqResult () ())),
      _statsClient :: Event t () -> m (Event t (ReqResult () (Map String String)))
  }

makeLenses ''BackendClient