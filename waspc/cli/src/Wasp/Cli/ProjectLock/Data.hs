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
    -- | If the holding process watches the project for changes and recompiles
    -- on them, this will contain its current status.
    watcherStatus :: Maybe WatcherStatus
  }
  deriving (Generic, FromJSON, ToJSON)

-- | What a process that watches the project for changes and recompiles on them
-- says about the state of the generated app right now.
data WatcherStatus
  = -- | It is compiling the project, so the generated app is being rewritten.
    Compiling
  | -- | The generated app is up to date with the project's source.
    UpToDate
  | -- | The last compilation failed, so the generated app is outdated until the
    -- user fixes the errors.
    CompilationFailed
  deriving (Generic, FromJSON, ToJSON)
