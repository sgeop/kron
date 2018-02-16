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

import Control.Monad.IO.Class(liftIO)

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
  deriving Show

DagRun
  dagId String
  scheduledStartTime UTCTime
|]

