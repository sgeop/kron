{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent(threadDelay)
import Data.Text(pack)
import System.Environment

import Task


mkTask1 = mkTask "task1" $ do
  threadDelay 3000000
  putStrLn "task1 done!"

mkTask2 = mkTask "task2" $ putStrLn "task2 done!"

mkTask3 = mkTask "task3" $ error "oops"

mkTask4 = mkTask "task4" $ putStrLn "task4 done!"

main :: IO ()
main = do
  (schedDate : xs) <- getArgs
  runDag "dag1" (pack schedDate) $ do
    task1 <- mkTask1 []
    task2 <- mkTask2 [task1]
    task3 <- mkTask3 [task1]
    task4 <- mkTask4 [task2, task3]
    pure [task1, task2, task3, task4]
