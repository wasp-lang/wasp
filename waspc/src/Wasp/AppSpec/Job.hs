{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.Job
  ( Job (..),
    JobExecutor (..),
    Perform (..),
    Schedule (..),
    ExecutorOptions (..),
    performExecutorOptionsJson,
    scheduleExecutorOptionsJson,
    jobExecutors,
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (JSON (..))

data Job = Job
  { executor :: JobExecutor,
    perform :: Perform,
    schedule :: Maybe Schedule,
    entities :: Maybe [Ref Entity]
  }
  deriving (Show, Eq, Data)

instance IsDecl Job

data JobExecutor = Simple | PgBoss
  deriving (Show, Eq, Data, Ord, Enum, Bounded)

data Perform = Perform
  { fn :: ExtImport,
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data)

instance IsDecl Perform

-- Allows jobs to run via some cron schedule.
data Schedule = Schedule
  { cron :: String, -- 5 field cron expression, exe: "*/5 * * * *".
    args :: Maybe JSON, -- Arguments to pass to the job handler function (`Perform.fn`).
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data)

instance IsDecl Schedule

-- These are optional executor-specific JSON options we pass
-- directly through to the executor when submitting jobs.
data ExecutorOptions = ExecutorOptions
  { pgBoss :: Maybe JSON,
    simple :: Maybe JSON
  }
  deriving (Show, Eq, Data)

jobExecutors :: [JobExecutor]
jobExecutors = enumFrom minBound :: [JobExecutor]

-- Helpers to disambiguate duplicate field `executorOptions`.
performExecutorOptionsJson :: Job -> Maybe JSON
performExecutorOptionsJson job =
  executorOptionsJson (executor job) (executorOptions (perform job :: Perform))

scheduleExecutorOptionsJson :: Job -> Maybe JSON
scheduleExecutorOptionsJson job = do
  s <- schedule job
  executorOptionsJson (executor job) (executorOptions (s :: Schedule))

executorOptionsJson :: JobExecutor -> Maybe ExecutorOptions -> Maybe JSON
executorOptionsJson Simple (Just ExecutorOptions {simple = Just json}) = Just json
executorOptionsJson PgBoss (Just ExecutorOptions {pgBoss = Just json}) = Just json
executorOptionsJson _ _ = Nothing
