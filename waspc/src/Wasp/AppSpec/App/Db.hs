{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Db
  ( Db (..),
    DbSystem (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.ExtImport (ExtImport)

data Db = Db
  { system :: Maybe DbSystem,
    seeds :: Maybe [ExtImport],
    prismaPreviewFeatures :: Maybe [String]
  }
  deriving (Show, Eq, Data)

data DbSystem = PostgreSQL | SQLite
  deriving (Show, Eq, Data)
