{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Db
  ( Db (..),
    DbSystem (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)

data Db = Db
  { seeds :: Maybe [ExtImport],
    prismaSetupFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

data DbSystem = PostgreSQL | SQLite
  deriving (Show, Eq, Data, Generic, FromJSON)
