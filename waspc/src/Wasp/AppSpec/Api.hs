{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Api
  ( Api (..),
    HttpMethod (..),
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
    method :: HttpMethod,
    path :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl Api

data HttpMethod = ALL | GET | POST | PUT | DELETE
  deriving (Show, Eq, Data)
