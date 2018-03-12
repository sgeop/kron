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

import Data.Semigroup((<>))
import Data.Text(Text)

import Database.Beam as Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL
import Database.Beam.Backend.Types(Auto)
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField

import Lens.Micro

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


data DagRunT f = DagRun
  { _drId :: Columnar f Text
  , _drName :: Columnar f Text
  , _drScheduledDate :: Columnar f Text
  } deriving Generic

type DagRun = DagRunT Identity
deriving instance Show DagRun


instance Table DagRunT where
  data PrimaryKey DagRunT f =
    DagRunId (C f Text) deriving Generic

  primaryKey = DagRunId . _drId

type DagRunId = PrimaryKey DagRunT Identity

instance Beamable DagRunT
instance Beamable (PrimaryKey DagRunT)


-- lenses for DagRun fields
drId :: Lens' (DagRunT f) (C f Text)
drId f dr = (\a -> dr { _drId = a }) <$> f (_drId dr)

drName :: Lens' (DagRunT f) (C f Text)
drName f dr = (\a -> dr { _drName = a }) <$> f (_drName dr)

drScheduledDate :: Lens' (DagRunT f) (C f Text)
drScheduledDate f dr = 
  (\a -> dr { _drScheduledDate = a }) <$> f (_drScheduledDate dr)



data TaskRunT f = TaskRun
  { _trTaskId :: Columnar f Text
  , _trStatus :: Columnar f Status
  , _trForDagRun :: PrimaryKey DagRunT f
  } deriving Generic

type TaskRun = TaskRunT Identity
deriving instance Show (PrimaryKey DagRunT Identity)
deriving instance Show TaskRun

instance Table TaskRunT where
  data PrimaryKey TaskRunT f =
    TaskRunId (C f Text) deriving Generic

  primaryKey = TaskRunId . _trTaskId

type TaskRunId = PrimaryKey TaskRunT Identity


instance Beamable TaskRunT
instance Beamable (PrimaryKey TaskRunT)

-- lenses for TaskRun fields
trTaskId :: Lens' (TaskRunT f) (C f Text)
trTaskId f tr = (\a -> tr { _trTaskId = a }) <$> f (_trTaskId tr)

trStatus :: Lens' (TaskRunT f) (C f Status)
trStatus f tr = (\a -> tr { _trStatus = a }) <$> f (_trStatus tr)

trForDagRun :: Lens' (TaskRunT f) (PrimaryKey DagRunT f)
trForDagRun f tr = (\a -> tr { _trForDagRun = a}) <$> f (_trForDagRun tr)



data KronDb f = KronDb
  { _kronTaskRuns :: f (TableEntity TaskRunT)
  , _kronDagRuns  :: f (TableEntity DagRunT )
  } deriving Generic

instance Database KronDb

--lenses for KronDb
KronDb (TableLens kronTaskRuns) (TableLens kronDagRuns) = dbLenses

kronDb :: DatabaseSettings be KronDb
kronDb = defaultDbSettings


initDagRun :: Connection -> Text -> Text -> IO DagRun
initDagRun conn name schedDate = do
  prevDagRun <- lookupDagRun
  case prevDagRun of
    Just dagRun -> pure dagRun
    Nothing -> do
      let dagRun = DagRun idStr name schedDate
      liftIO $ withDatabase conn $
        runInsert $ insert (kronDb ^. kronDagRuns) $ insertValues [dagRun]
      pure dagRun
  where
    idStr = name <> "-" <> schedDate
    lookupDagRun = liftIO $ withDatabase conn $ runSelectReturningOne $
      Beam.lookup (kronDb ^. kronDagRuns) (DagRunId idStr)


initTaskRun :: Connection -> DagRun -> Text -> IO ()
initTaskRun conn dagRun taskId = liftIO $
  -- if (getStatus
  withDatabase conn $ runInsert $
  -- withDatabaseDebug putStrLn conn $ runInsert $
    insert (kronDb ^. kronTaskRuns) $
      insertValues [ TaskRun taskId Pending (pk dagRun)]


getTaskRun :: Connection -> DagRun -> Text -> IO (Maybe TaskRun)
getTaskRun conn dagRun taskId = liftIO $
   withDatabase conn $ runSelectReturningOne $ select $ do
     tr <- all_ (kronDb ^. kronTaskRuns)
     guard_ ((tr ^. trForDagRun ==. val_ dagRunId) 
         &&. (tr ^. trTaskId ==. val_ taskId))
     pure tr
   where dagRunId = DagRunId $ dagRun ^. drId


updateTaskRun :: Connection -> DagRun -> Text -> Status -> IO ()
updateTaskRun conn dagRun taskId status = liftIO $
  withDatabase conn $
  -- withDatabaseDebug putStrLn conn $
    runUpdate $ update (kronDb ^. kronTaskRuns)
                       (\tr -> [ tr ^. trStatus <-. val_ status ])
                       (\tr -> tr ^. trTaskId ==. val_ taskId 
                           &&. tr ^. trForDagRun ==. val_ dagRunId)
  where dagRunId = DagRunId $ dagRun ^. drId
