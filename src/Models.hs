{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoPatBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL
import Database.Beam.Backend.Types(Auto)
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField

import Lens.Micro

import Data.Text(Text)
import Text.Read(readMaybe)


-- Type Indicating current status of a Task Run
data Status
  = Pending
  | Running
  | Failed
  | UpstreamFailed
  | Succeeded
  deriving (Show, Read, Eq, Ord, Enum)


-- instance indicating how to convert haskell value into backend type
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
    sqlValueSyntax = autoSqlValueSyntax


--instance indicating how to convert database type into haskell value
instance FromBackendRow Sqlite Status

instance FromField Status where
  fromField f = do
    x <- readMaybe <$> fromField f
    case x of
      Nothing -> returnError
        ConversionFailed f "Could not 'read' value for 'Status'"
      Just x -> pure x


data TaskRunT f = TaskRun
  { _trId :: C f (Auto Int)
  , _trTaskId :: C f Text
  , _trStatus :: C f Status
  } deriving Generic
deriving instance Show TaskRun

type TaskRun = TaskRunT Identity


instance Table TaskRunT where
  data PrimaryKey TaskRunT f = TaskRunId (C f (Auto Int)) deriving Generic
  primaryKey = TaskRunId . _trId
type TaskRunId = PrimaryKey TaskRunT Identity


instance Beamable TaskRunT
instance Beamable (PrimaryKey TaskRunT)


-- lenses for TaskRunT
trId :: Lens' (TaskRunT f) (C f (Auto Int))
trId f tr = (\a -> tr { _trId = a }) <$> f (_trId tr)

trTaskId :: Lens' (TaskRunT f) (C f Text)
trTaskId f tr = (\a -> tr { _trTaskId = a }) <$> f (_trTaskId tr)

trStatus :: Lens' (TaskRunT f) (C f Status)
trStatus f tr = (\a -> tr { _trStatus = a }) <$> f (_trStatus tr)


newtype KronDb f = KronDb
  { _kronTaskRuns :: f (TableEntity TaskRunT)
  } deriving Generic

instance Database KronDb

--lenses for KronDb
KronDb (TableLens kronTaskRuns) = dbLenses

kronDb :: DatabaseSettings be KronDb
kronDb = defaultDbSettings


initTaskRun :: Connection -> Text -> IO ()
initTaskRun conn taskId = liftIO $
  withDatabase conn $ runInsert $
  -- withDatabaseDebug putStrLn conn $ runInsert $
    insert (kronDb ^. kronTaskRuns) $
      insertValues [ TaskRun (Auto Nothing) taskId Pending ]


updateTaskRun :: Connection -> Text -> Status -> IO ()
updateTaskRun conn taskId status = liftIO $
  withDatabase conn $
  -- withDatabaseDebug putStrLn conn $
    runUpdate $ update (kronDb ^. kronTaskRuns)
                       (\tr -> [ tr ^. trStatus <-. val_ status ])
                       (\tr -> tr ^. trTaskId ==. val_ taskId)
