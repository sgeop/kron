{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField

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

instance FromField Status where
  fromField f = do
    x <- readMaybe <$> fromField f
    case x of
      Nothing -> returnError
        ConversionFailed f "Could not 'read' value for 'Status'"
      Just x -> pure x

--instance indicating how to convert database type into haskell value
instance FromBackendRow be Status


data TaskRunT f = TaskRun
  { _taskRunId :: Columnar f Int
  , _taskRunTaskDefId :: Columnar f Text
  , _taskRunStatus :: Columnar f Status
  } deriving Generic

type TaskRun = TaskRunT Identity
type TaskRunId = PrimaryKey TaskRunT Identity

deriving instance Show TaskRun
deriving instance Eq TaskRun

instance Beamable  TaskRunT

instance Table TaskRunT where
  data PrimaryKey TaskRunT f = TaskRunId (Columnar f Int) deriving Generic
  primaryKey = TaskRunId . _taskRunId
instance Beamable (PrimaryKey TaskRunT)

newtype KronDb f = KronDb
  { _kronTaskRun :: f (TableEntity TaskRunT)
  } deriving Generic

instance Database KronDb

kronDb :: DatabaseSettings be KronDb
kronDb = defaultDbSettings

insertTaskRuns :: Connection -> IO ()
insertTaskRuns conn = liftIO $ withDatabase conn $ runInsert $
  insert (_kronTaskRun kronDb) $
    insertValues [ TaskRun 1 "task1" Pending ]


