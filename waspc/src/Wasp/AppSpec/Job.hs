{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)
import Wasp.AppSpec.JSON (JSON (..))

data Job = Job
  { executor :: JobExecutor,
    perform :: Perform,
    schedule :: Maybe Schedule,
    entities :: Maybe [Ref Entity]
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance IsDecl Job

instance Aeson.ToJSON Job where
  toJSON job =
    let requiredFields =
          [ "executor" Aeson..= executor job,
            "perform" Aeson..= perform job
          ]
        optionalFields =
          [ maybeToField "schedule" (schedule job),
            maybeToField "entities" (entities job)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)

data JobExecutor = PgBoss
  deriving (Show, Eq, Data, Ord, Enum, Bounded, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Perform = Perform
  { fn :: ExtImport,
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON Perform where
  toJSON perf =
    let requiredFields = ["fn" Aeson..= fn perf]
        optionalFields =
          [ maybeToField "executorOptions" (executorOptions (perf :: Perform))
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)

-- Allows jobs to run via some cron schedule.
data Schedule = Schedule
  { cron :: String, -- 5 field cron expression, exe: "*/5 * * * *".
    args :: Maybe JSON, -- Arguments to pass to the job handler function (`Perform.fn`).
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON Schedule where
  toJSON sched =
    let requiredFields = ["cron" Aeson..= cron sched]
        optionalFields =
          [ maybeToField "args" (args sched),
            maybeToField "executorOptions" (executorOptions (sched :: Schedule))
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)

-- These are optional executor-specific JSON options we pass
-- directly through to the executor when submitting jobs.
data ExecutorOptions = ExecutorOptions
  { pgBoss :: Maybe JSON
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON ExecutorOptions where
  toJSON executorOpts =
    let optionalFields =
          [ maybeToField "pgBoss" (pgBoss executorOpts)
          ]
     in Aeson.object (catMaybes optionalFields)

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
executorOptionsJson PgBoss (Just ExecutorOptions {pgBoss = Just json}) = Just json
executorOptionsJson _ _ = Nothing
