{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.Job
  ( Job (..),
    JobExecutor (..),
    Perform (..),
    Schedule (..),
    performExecutorOptionsJson,
    scheduleExecutorOptionsJson,
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
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data)

instance IsDecl Perform

data Schedule = Schedule
  { cron :: String,
    args :: Maybe JSON,
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data)

instance IsDecl Schedule

data ExecutorOptions = ExecutorOptions
  { pgBoss :: Maybe JSON,
    passthrough :: Maybe JSON
  }
  deriving (Show, Eq, Data)

jobExecutors :: [JobExecutor]
jobExecutors = enumFrom minBound :: [JobExecutor]

-- Helpers to disambiguate duplicate field `executorOptions`.
performExecutorOptionsJson :: Job -> Maybe JSON
performExecutorOptionsJson job =
  executorOptionsJson (executor job) (executorOptions (perform job :: Perform))

scheduleExecutorOptionsJson :: Job -> Maybe JSON
scheduleExecutorOptionsJson job =
  case schedule job of
    Nothing -> Nothing
    Just s -> executorOptionsJson (executor job) (executorOptions (s :: Schedule))

executorOptionsJson :: JobExecutor -> Maybe ExecutorOptions -> Maybe JSON
executorOptionsJson Passthrough (Just ExecutorOptions {passthrough = Just json}) = Just json
executorOptionsJson PgBoss (Just ExecutorOptions {pgBoss = Just json}) = Just json
executorOptionsJson _ _ = Nothing
