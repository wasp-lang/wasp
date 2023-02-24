{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Api
  ( Api (..),
    HttpVerb (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity
import Wasp.AppSpec.ExtImport

data Api = Api
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    verb :: HttpVerb,
    route :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl Api

data HttpVerb = ALL | GET | POST | PUT | DELETE
  deriving (Show, Eq, Data)
