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
import Data.Functor(($>))
import Data.Text(Text, unpack)
import Database.SQLite.Simple
import Models


data Task = Task
  { _taskId :: Text
  , _run :: Connection -> DagRun -> IO ()
  , _status :: MVar Status
  , _dependencies :: [MVar Status]
  }

runTask :: Text -> IO () -> MVar Status -> [MVar Status] -> Connection -> DagRun -> IO ()
runTask taskId exec status deps conn dagRun = do
  forkIO (do
    result <- execTask
    updateTaskRun conn dagRun taskId result
    putMVar status result)
  pure ()
  where
    execTask = do
      prevTr <- getTaskRun conn dagRun taskId
      putStrLn ("Status: " ++ show prevTr)
      if (_trStatus <$> prevTr) == Just Succeeded
      then putStrLn "already finished, skipping.." $> Succeeded
      else do
        depsState <- traverse readMVar deps
        case prevTr of
          Nothing -> initTaskRun conn dagRun taskId
          _ -> updateTaskRun conn dagRun taskId Pending
        if Failed `elem` depsState
        then return UpstreamFailed
        else do
           updateTaskRun conn dagRun taskId Running
           putStrLn ("Running task: " ++ unpack taskId)
           exec
           pure Succeeded
         `catch` \(e :: SomeException) -> return Failed
    



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
  traverse (\t -> _run t conn dagRun) tasks'
  results <- traverse (readMVar . _status) tasks'
  print results
