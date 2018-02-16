module Main where

import Control.Concurrent(threadDelay)

import Task


mkTask1 = mkTask "task1" $ do
  threadDelay 3000000
  putStrLn "task1 done!"

mkTask2 = mkTask "task2" $ putStrLn "task2 done!"

mkTasks :: IO [Task]
mkTasks = do
  task1 <- mkTask1 []
  task2 <- mkTask2 [task1]
  pure [task1, task2]

main = runDag mkTasks
