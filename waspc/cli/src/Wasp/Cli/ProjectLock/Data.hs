{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.ProjectLock.Data
  ( ProjectLockData (..),
    WatcherStatus (..),
    WaspProcessId,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

type WaspProcessId = Integer

-- | Information for the other processes that find the project locked.
data ProjectLockData = ProjectLockData
  { pid :: WaspProcessId,
    watcherStatus :: Maybe WatcherStatus
  }
  deriving (Generic, FromJSON, ToJSON)

data WatcherStatus = Compiling | UpToDate | CompilationFailed
  deriving (Generic, FromJSON, ToJSON)
