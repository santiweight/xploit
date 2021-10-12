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

import qualified BasicPrelude                  as P
import GameLogic
import           Common.Server.Api              ( Add
                                                , LoadHandHAPI
                                                , PokerAPI, NodeQueryResponse
                                                , NodeQueryRequest
                                                )
import           Control.Lens                   ( (<&>)
                                                , (^.)
                                                , makeLenses
                                                )
import           Control.Monad.Fix
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Bifunctor
import           Data.Data                      ( Proxy(Proxy) )
import           Data.Functor.Identity          ( Identity(Identity) )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           JSDOM.Generated.Element        ( Element(unElement)
                                                , IsElement
                                                , toElement
                                                )
import           JSDOM.Types                    ( JSM
                                                , liftJSM
                                                )
import           Language.Javascript.JSaddle    ( (!)
                                                , (#)
                                                , fun
                                                , function
                                                , nextAnimationFrame
                                                )
import           Language.Javascript.JSaddle.Evaluate
                                                ( eval )
import           Language.Javascript.JSaddle.Object
                                                ( jsg )
import           Language.Javascript.JSaddle.Value
                                                ( valToText )
import           Poker( BetAction
                                                  ( AllInRaise
                                                  , Call
                                                  , Check
                                                  , Fold
                                                  , Raise
                                                  )
                                                , Position
                                                )
import           Reflex.Dom
import           Servant.API                    ( type (:<|>)(..) )
import           Servant.Reflex                 ( BaseUrl(BasePath)
                                                , QParam
                                                , ReqResult
                                                , client
                                                )
import           Text.Megaparsec
import           Text.Megaparsec.Char           ( char )
import           Text.Megaparsec.Char.Lexer
import           Text.Megaparsec.Internal       ( ParsecT(ParsecT) )
import Obelisk.Frontend
import Obelisk.Route
import Common.Route
import Control.Monad
import Control.Monad.State (evalStateT)

loadHandClient :: forall t m. MonadWidget t m => AddHandClient t m
loadHandClient = do
  let ((_loadFiles :<|> _loadDir) :<|> _addHandContents) = client
        (Proxy :: Proxy Add)
        (Proxy :: Proxy m)
        (Proxy :: Proxy ())
        (constDyn (BasePath "/"))
  undefined

backendClient :: forall t m . MonadWidget t m => BackendClient t m
backendClient = do
  let (_queryApi :<|> _ :<|> _echo) = client (Proxy :: Proxy PokerAPI)
                                             (Proxy :: Proxy m)
                                             (Proxy :: Proxy ())
                                             (constDyn (BasePath "/"))
  MyClient _queryApi loadHandClient _echo

data AddHandClient t m = AddHandClient
  { addFilesApi
      :: Dynamic t (QParam [FilePath])
      -> Event t ()
      -> m (Event t (ReqResult () ()))
  , _addDirApi
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
      :: Dynamic t (Either Text Int)
      -> Event t ()
      -> m (Event t (ReqResult () ()))
  }

makeLenses ''BackendClient