{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Job
  ( Job (..),
    JobExecutor (..),
    Perform (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (JSON)

data Job = Job
  { executor :: JobExecutor,
    perform :: Perform
  }
  deriving (Show, Eq, Data)

instance IsDecl Job

data JobExecutor = PgBoss
  deriving (Show, Eq, Data)

data Perform = Perform
  { fn :: ExtImport,
    options :: Maybe JSON
  }
  deriving (Show, Eq, Data)

instance IsDecl Perform
