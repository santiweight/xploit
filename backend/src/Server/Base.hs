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
import Common.DB.Instances ()
import Common.Server.Api (ReviewHistory)
import Control.Monad.Logger
  ( LoggingT,
    runStdoutLoggingT,
  )
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
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
import Poker
import Poker.History.Bovada.Model
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
  ReviewHistoryP
    serial ByteString -- @ReviewHistory
    deriving Show Read
|]

connString :: ConnectionString
connString = "host=127.0.0.1 user=postgres dbname=xploitdb password=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = do
  runStdoutLoggingT $
    withPostgresqlConn connectionString $ \backend ->
      runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)

insertHand :: History (Amount "USD") -> IO ()
insertHand hand = do
  -- migrateDB
  -- print "inserting hand"
  -- print hand
  tz <- getCurrentTimeZone
  (() <$) . runAction connString . insertBy . toHandH tz $ hand
  where
    toHandH :: TimeZone -> History (Amount "USD") -> HandH
    toHandH tz h@History {..} =
      HandH
        { handHHandId = gameId header,
          handHHandHistoryText = _handText,
          handHTableTy = gameTy header,
          handHTime = localTimeToUTC tz $ time header,
          handHSerial = BL.toStrict $ serialise h
        }

insertReview :: ReviewHistory -> IO ReviewHistoryPId
insertReview hist = do
  runAction connString . insert . toReviewHistoryP $ hist
  where
    toReviewHistoryP :: ReviewHistory -> ReviewHistoryP
    toReviewHistoryP = ReviewHistoryP . BL.toStrict . serialise
