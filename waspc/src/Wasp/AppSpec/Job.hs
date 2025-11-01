{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

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

import Data.Aeson (FromJSON, parseJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
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
  deriving (Show, Eq, Data, Generic, FromJSON)

instance IsDecl Job

data JobExecutor = PgBoss
  deriving (Show, Eq, Data, Ord, Enum, Bounded, Generic)

-- NOTE: For some reason, deriving FromJSON for JobExecutor does not work. I'm
-- guessing it's because "PgBoss" is the only data constructor (the same code
-- works as expected with EmailProvider, which is very similar).
instance FromJSON JobExecutor where
  parseJSON executorStr = case executorStr of
    "PgBoss" -> pure PgBoss
    _ -> fail $ "Failed to parse job executor: " <> show executorStr

data Perform = Perform
  { fn :: ExtImport,
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

-- Allows jobs to run via some cron schedule.
data Schedule = Schedule
  { cron :: String, -- 5 field cron expression, exe: "*/5 * * * *".
    args :: Maybe JSON, -- Arguments to pass to the job handler function (`Perform.fn`).
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

-- These are optional executor-specific JSON options we pass
-- directly through to the executor when submitting jobs.
data ExecutorOptions = ExecutorOptions
  { pgBoss :: Maybe JSON
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

jobExecutors :: [JobExecutor]
jobExecutors = enumFrom minBound :: [JobExecutor]

-- Helpers to disambiguate duplicate field `executorOptions`.
performExecutorOptionsJson :: Job -> Maybe JSON
performExecutorOptionsJson job =
  executorOptionsJson (executor job) job.perform.executorOptions

scheduleExecutorOptionsJson :: Job -> Maybe JSON
scheduleExecutorOptionsJson job = do
  s <- schedule job
  executorOptionsJson (executor job) s.executorOptions

executorOptionsJson :: JobExecutor -> Maybe ExecutorOptions -> Maybe JSON
executorOptionsJson PgBoss (Just ExecutorOptions {pgBoss = Just json}) = Just json
executorOptionsJson _ _ = Nothing
