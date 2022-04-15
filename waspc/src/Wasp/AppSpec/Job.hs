{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Job
  ( Job (..),
    Perform (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (JSON)

data Job = Job
  { perform :: Perform,
    concurrency :: Maybe Integer
  }
  deriving (Show, Eq, Data)

instance IsDecl Job

data Perform = Perform
  { fn :: ExtImport,
    options :: Maybe JSON
  }
  deriving (Show, Eq, Data)

instance IsDecl Perform
