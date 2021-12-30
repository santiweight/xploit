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

module Server.DB.Schema where

import Common.DB.Instances ()
import Control.Monad.Logger
  ( LoggingT,
    runStdoutLoggingT,
  )
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time
  ( UTCTime,
  )
import Database.Persist.Postgresql
import Database.Persist.TH as PTH
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

