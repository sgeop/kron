{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB where

import Control.Monad.Reader
import Control.Monad.IO.Class(liftIO)

import Data.Int(Int64)
import Data.Time

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Status


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TaskRun
  taskId String
  startTime UTCTime Maybe
  endTime UTCTime Maybe
  status Status
  dagRun DagRunId
  DagRunTask taskId dagRun
  deriving Show

DagRun
  scheduledStartTime UTCTime
|]

asSqlBackend :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackend = id


initDagRun sst = insert $
  DagRun sst

initTaskRun :: String -> Key DagRun -> IO (Key TaskRun)
initTaskRun tId drId = runSqlite ":memory:" . asSqlBackend $ do
    trId <- insert $ TaskRun tId Nothing Nothing Pending drId
    pure trId

updateTaskRun trId status = update trId [TaskRunStatus =. status]
