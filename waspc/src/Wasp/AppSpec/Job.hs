{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.Job
  ( Job (..),
    JobExecutor (..),
    Perform (..),
    Schedule (..),
    performOptions,
    sheduleOptions,
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
    options :: Maybe JSON
  }
  deriving (Show, Eq, Data)

instance IsDecl Perform

data Schedule = Schedule
  { cron :: String,
    performFnArg :: Maybe JSON,
    options :: Maybe JSON
  }
  deriving (Show, Eq, Data)

instance IsDecl Schedule

jobExecutors :: [JobExecutor]
jobExecutors = enumFrom minBound :: [JobExecutor]

-- Helpers to disambiguate duplicate field `option`.
performOptions :: Perform -> Maybe JSON
performOptions p = options (p :: Perform)

sheduleOptions :: Schedule -> Maybe JSON
sheduleOptions s = options (s :: Schedule)
