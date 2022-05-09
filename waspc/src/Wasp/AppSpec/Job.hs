{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.Job
  ( Job (..),
    JobExecutor (..),
    Perform (..),
    Schedule (..),
    performExecutorOptions,
    scheduleExecutorOptions,
    jobExecutors,
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (JSON (..))

data Job = Job
  { executor :: JobExecutor,
    perform :: Perform,
    schedule :: Maybe Schedule
  }
  deriving (Show, Eq, Data)

instance IsDecl Job

data JobExecutor = Passthrough | PgBoss
  deriving (Show, Eq, Data, Ord, Enum, Bounded)

data Perform = Perform
  { fn :: ExtImport,
    executorOptions :: Maybe JSON
  }
  deriving (Show, Eq, Data)

instance IsDecl Perform

data Schedule = Schedule
  { cron :: String,
    args :: Maybe JSON,
    executorOptions :: Maybe JSON
  }
  deriving (Show, Eq, Data)

instance IsDecl Schedule

jobExecutors :: [JobExecutor]
jobExecutors = enumFrom minBound :: [JobExecutor]

-- Helpers to disambiguate duplicate field `options`.
performExecutorOptions :: Perform -> Maybe JSON
performExecutorOptions p = executorOptions (p :: Perform)

scheduleExecutorOptions :: Schedule -> Maybe JSON
scheduleExecutorOptions s = executorOptions (s :: Schedule)
