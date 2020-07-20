{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.Base where

import Codec.Serialise (serialise)
import Control.Monad.Logger
  ( LoggingT,
    runStdoutLoggingT,
  )
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
  ( TimeZone,
    UTCTime,
  )
import Data.Time.LocalTime
  ( getCurrentTimeZone,
    localTimeToUTC,
  )
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH as PTH
import Poker.Base
  ( GameType (..),
    Hand (..),
  )

import Common.DB.Instances ()
import Server.Instances ()

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  HandH sql=hands
    handId Int
    time UTCTime
    tableTy GameType
    handHistoryText Text
    serial ByteString -- @Hand (not [Action])
    UniqueHandID handId
    deriving Show Read
|]

connString :: ConnectionString
connString = "host=127.0.0.1 user=xploit dbname=xploitdb"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)

insertHand :: Hand -> IO ()
insertHand hand = do
  -- migrateDB
  -- print "inserting hand"
  -- print hand
  tz <- getCurrentTimeZone
  (() <$) . runAction connString . insertBy . toHandH tz $ hand
  where
    toHandH :: TimeZone -> Hand -> HandH
    toHandH tz h@Hand {..} =
      HandH
        { handHHandId = _handID,
          handHHandHistoryText = T.pack _handText,
          handHTableTy = _handTy,
          handHTime = localTimeToUTC tz _handTime,
          handHSerial = L.toStrict $ serialise h
        }