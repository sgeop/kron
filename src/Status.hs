{-# LANGUAGE TemplateHaskell #-}

module Status where

import Database.Persist.TH

data Status
  = Pending
  | Running
  | Failed
  | UpstreamFailed
  | Succeeded
  deriving (Show, Read, Eq)

derivePersistField "Status"
