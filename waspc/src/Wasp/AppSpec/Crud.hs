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
import Data.List (intercalate)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (..))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data Crud = Crud
  { entity :: Ref Entity,
    operations :: CrudOperations
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Crud

instance Inspectable Crud where
  inspectionSection = "CRUDs"
  inspect (name, crud) =
    InspectionEntry
      [ name,
        "on " ++ refName (entity crud),
        intercalate ", " $ showEnabledOperations $ operations crud
      ]
    where
      showEnabledOperations ops =
        [ showOperation operationName options
        | (operationName, Just options) <-
            [ ("get", get ops),
              ("getAll", getAll ops),
              ("create", create ops),
              ("update", update ops),
              ("delete", delete ops)
            ]
        ]
      showOperation operationName options =
        operationName
          ++ (if isPublic options == Just True then " (public)" else "")
          ++ (if isJust (overrideFn options) then " (override)" else "")

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
