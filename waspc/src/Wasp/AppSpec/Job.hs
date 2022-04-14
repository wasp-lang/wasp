{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Job
  ( Job (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (JSON)

data Job = Job
  { perform :: ExtImport,
    options :: Maybe JSON
  }
  deriving (Show, Eq, Data)

instance IsDecl Job
