module Main where

import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , readMVar
  )

import DB
import Status

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


mkTask1 = mkTask "task1" $ do
  threadDelay 3000000
  putStrLn "task1 done!"

mkTask2 = mkTask "task2" $ putStrLn "task2 done!"



main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  task1 <- mkTask1 []
  task2 <- mkTask2 [task1]
  let tasks = [task1, task2]
  traverse _run tasks
  results <- traverse (readMVar . _status) tasks
  print results
