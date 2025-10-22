{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Crud
  ( Crud (..),
    CrudOperations (..),
    CrudOperation (..),
    CrudOperationOptions (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data Crud = Crud
  { entity :: Ref Entity,
    operations :: CrudOperations
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Crud

data CrudOperations = CrudOperations
  { get :: Maybe CrudOperationOptions,
    getAll :: Maybe CrudOperationOptions,
    create :: Maybe CrudOperationOptions,
    update :: Maybe CrudOperationOptions,
    delete :: Maybe CrudOperationOptions
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data CrudOperationOptions = CrudOperationOptions
  { isPublic :: Maybe Bool,
    overrideFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data CrudOperation = Get | GetAll | Create | Update | Delete
  deriving (Show, Eq, Ord, Data, Generic, FromJSON, ToJSON)
