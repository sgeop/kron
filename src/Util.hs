module Util where

import Data.Char(toLower)

dropPrefix :: String -> String -> String
dropPrefix prefix =
  lowerFirst . drop (length prefix)
  where
    lowerFirst (x:xs) = toLower x : xs

