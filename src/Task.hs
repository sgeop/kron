{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task where

import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , readMVar
  )
import Control.Exception
import Data.Text(Text)
import Database.SQLite.Simple
import Models


data Config = Config { conn :: Connection }

data Task = Task
  { _taskId :: Text
  , _run :: Connection -> DagRun -> IO ()
  , _status :: MVar Status
  , _dependencies :: [MVar Status]
  }

runTask :: Text -> IO () -> MVar Status -> [MVar Status] -> Connection -> DagRun -> IO ()
runTask taskId exec status deps conn dagRun = do
  forkIO (do
    print dagRun
    initTaskRun conn dagRun taskId
    depState <- traverse readMVar deps
    updateTaskRun conn taskId Running
    result <- if any (== Failed) depState
              then return UpstreamFailed
              else (exec *> pure Succeeded)
                `catch` \(e :: SomeException) -> return Failed
    updateTaskRun conn taskId result
    putMVar status result)
  pure ()


mkTask :: Text -> IO () -> [Task] -> IO Task
mkTask taskId exec deps = do
  status <- newEmptyMVar
  let dependencies = fmap _status deps
  return Task
    { _taskId = taskId
    , _run = runTask taskId exec status dependencies
    , _status = status
    , _dependencies = dependencies
    }


runDag :: Text -> Text -> IO [Task] -> IO ()
runDag dagName schedDate tasks = do
  conn <- open "kron.db"
  dagRun <- initDagRun conn dagName schedDate
  tasks' <- tasks
  traverse (\t -> (_run t) conn dagRun) tasks'
  results <- traverse (readMVar . _status) tasks'
  print results
