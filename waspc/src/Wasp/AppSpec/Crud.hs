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

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isJust)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
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
  inspect crud =
    [ InspectionEntry "CRUDs" $
        ("Entity", refName (entity crud))
          : showEnabledOperations (operations crud)
    ]
    where
      showEnabledOperations ops =
        [ (show operationName, showOperation operationOptions)
        | (operationName, operationOptions) <- toOperationList ops
        ]

      showOperation options =
        unwords
          [ "Enabled",
            wrapInParens $
              intercalate ", " $
                ["public" | isPublic options == Just True]
                  ++ ["overridden" | isJust (overrideFn options)]
          ]

      wrapInParens "" = ""
      wrapInParens s = "(" ++ s ++ ")"

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
  deriving (Show, Eq, Ord, Data, Generic, FromJSON, ToJSON, Enum, Bounded)

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
