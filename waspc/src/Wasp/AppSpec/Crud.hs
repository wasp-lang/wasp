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

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data Crud = Crud
  { entity :: Ref Entity,
    operations :: CrudOperations
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON, Aeson.ToJSON)

instance IsDecl Crud

data CrudOperations = CrudOperations
  { get :: Maybe CrudOperationOptions,
    getAll :: Maybe CrudOperationOptions,
    create :: Maybe CrudOperationOptions,
    update :: Maybe CrudOperationOptions,
    delete :: Maybe CrudOperationOptions
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON CrudOperations where
  toJSON ops =
    let optionalFields =
          [ maybeToField "get" (get ops),
            maybeToField "getAll" (getAll ops),
            maybeToField "create" (create ops),
            maybeToField "update" (update ops),
            maybeToField "delete" (delete ops)
          ]
     in Aeson.object (catMaybes optionalFields)

data CrudOperationOptions = CrudOperationOptions
  { isPublic :: Maybe Bool,
    overrideFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON CrudOperationOptions where
  toJSON opts =
    let optionalFields =
          [ maybeToField "isPublic" (isPublic opts),
            maybeToField "overrideFn" (overrideFn opts)
          ]
     in Aeson.object (catMaybes optionalFields)

data CrudOperation = Get | GetAll | Create | Update | Delete
  deriving (Show, Eq, Ord, Data, Generic, Aeson.FromJSON, Aeson.ToJSON)
