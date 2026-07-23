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

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Data (Data)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImportFromProjectDir)
import Wasp.AppSpec.JSON (JSON (..))

data Job = Job
  { executor :: JobExecutor,
    perform :: Perform,
    schedule :: Maybe Schedule,
    entities :: Maybe [Ref Entity]
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Job

instance Inspectable Job where
  inspect job =
    [ InspectionEntry "Jobs" $
        [ ("Executor", show $ executor job),
          ("Schedule", maybe "" (show . cron) (schedule job)),
          ("Import", showExtImportFromProjectDir job.perform.fn)
        ]
          ++ [("Entities", (intercalate ", " . fmap refName) entities') | Just entities' <- [entities job]]
    ]

data JobExecutor = PgBoss
  deriving (Show, Eq, Data, Ord, Enum, Bounded, Generic)

-- NOTE: For some reason, deriving FromJSON for JobExecutor does not work. I'm
-- guessing it's because "PgBoss" is the only data constructor (the same code
-- works as expected with EmailProvider, which is very similar).
instance FromJSON JobExecutor where
  parseJSON executorStr = case executorStr of
    "PgBoss" -> pure PgBoss
    _ -> fail $ "Failed to parse job executor: " <> show executorStr

-- NOTE: Hand-written for the same single-data-constructor reason as FromJSON above.
instance ToJSON JobExecutor where
  toJSON PgBoss = "PgBoss"

data Perform = Perform
  { fn :: ExtImport,
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- Allows jobs to run via some cron schedule.
data Schedule = Schedule
  { cron :: String, -- 5 field cron expression, exe: "*/5 * * * *".
    args :: Maybe JSON, -- Arguments to pass to the job handler function (`Perform.fn`).
    executorOptions :: Maybe ExecutorOptions
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- These are optional executor-specific JSON options we pass
-- directly through to the executor when submitting jobs.
data ExecutorOptions = ExecutorOptions
  { pgBoss :: Maybe JSON
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

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
