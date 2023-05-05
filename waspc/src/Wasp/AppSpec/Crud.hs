{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Crud
  ( Crud (..),
    CrudOperation (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)

data Crud = Crud
  { entity :: Ref Entity,
    only :: Maybe [CrudOperation],
    except :: Maybe [CrudOperation]
  }
  deriving (Show, Eq, Data)

instance IsDecl Crud

data CrudOperation = Get | GetAll | Create | Update | Delete
  deriving (Show, Eq, Ord, Data)
