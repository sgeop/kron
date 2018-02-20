{-# Language ScopedTypeVariables #-}

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
  , _run :: IO ()
  , _status :: MVar Status
  , _dependencies :: [MVar Status]
  }

runTask :: Text -> IO () -> MVar Status -> [MVar Status] -> IO ()
runTask taskId exec status deps = do
  forkIO (do
    conn <- open "kron.db"
    initTaskRun conn taskId
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


runDag :: IO [Task] -> IO ()
runDag tasks = do
  tasks' <- tasks
  traverse _run tasks'
  results <- traverse (readMVar . _status) tasks'
  print results
