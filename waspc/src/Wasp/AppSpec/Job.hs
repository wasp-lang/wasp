{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Job
  ( Job (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)

data Job = Job
  { perform :: ExtImport
  }
  deriving (Show, Eq, Data)

instance IsDecl Job
