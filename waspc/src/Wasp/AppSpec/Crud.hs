{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Wasp.AppSpec.Crud
  ( Crud (..),
    CrudOperations (..),
    CrudOperation (..),
    CrudOperationOptions (..),
    toOperationList,
  )
where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data Crud = Crud
  { entity :: Ref Entity,
    operations :: CrudOperations
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

instance IsDecl Crud

data CrudOperations = CrudOperations
  { get :: Maybe CrudOperationOptions,
    getAll :: Maybe CrudOperationOptions,
    create :: Maybe CrudOperationOptions,
    update :: Maybe CrudOperationOptions,
    delete :: Maybe CrudOperationOptions
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

data CrudOperationOptions = CrudOperationOptions
  { isPublic :: Maybe Bool,
    overrideFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

data CrudOperation = Get | GetAll | Create | Update | Delete
  deriving (Show, Eq, Ord, Data, Generic, FromJSON, Enum, Bounded)

toOperationList :: CrudOperations -> [(CrudOperation, CrudOperationOptions)]
toOperationList ops =
  catMaybes
    [ (operation,) <$> getOptions operation ops
    | operation <- [minBound ..] :: [CrudOperation]
    ]
  where
    getOptions Get = get
    getOptions GetAll = getAll
    getOptions Create = create
    getOptions Update = update
    getOptions Delete = delete
