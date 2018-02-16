module Task where

import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , readMVar
  )

import Status

-- data Config = Config { k

data Task = Task
  { _taskId :: String
  , _run :: IO ()
  , _status :: MVar Status
  , _dependencies :: [MVar Status]
  }

runTask :: IO () -> MVar Status -> [MVar Status] -> IO ()
runTask exec status deps = do
  forkIO (do
    traverse readMVar deps
    exec
    putMVar status Succeeded)
  return ()


mkTask :: String -> IO () -> [Task] -> IO Task
mkTask taskId exec deps = do
  status <- newEmptyMVar
  let dependencies = fmap _status deps
  return Task
    { _taskId = taskId
    , _run = runTask exec status dependencies
    , _status = status
    , _dependencies = dependencies
    }


runDag :: IO [Task] -> IO ()
runDag tasks = do
  tasks' <- tasks
  traverse _run tasks'
  results <- traverse (readMVar . _status) tasks'
  print results
