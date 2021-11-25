{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Query
  ( Query (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity
import Wasp.AppSpec.ExtImport

data Query = Query
  { fn :: ExtImport,
    entities :: [Ref Entity]
  }
  deriving (Show, Eq, Data)

instance IsDecl Query
