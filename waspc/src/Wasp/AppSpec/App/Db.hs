{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Db
  ( Db (..),
    DbSystem (..),
    Prisma (..)
  )
where

import Data.Data (Data)
import Wasp.AppSpec.ExtImport (ExtImport)

data Db = Db
  { system :: Maybe DbSystem,
    seeds :: Maybe [ExtImport],
    prisma :: Maybe Prisma
  }
  deriving (Show, Eq, Data)

data DbSystem = PostgreSQL | SQLite
  deriving (Show, Eq, Data)

data Prisma = Prisma
  { previewFeatures :: Maybe [String]
  }
  deriving (Show, Eq, Data)
