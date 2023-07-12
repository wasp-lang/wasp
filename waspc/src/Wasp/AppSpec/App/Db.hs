{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Db
  ( Db (..),
    DbSystem (..),
    PrismaOptions (..)
  )
where

import Data.Data (Data)
import Wasp.AppSpec.ExtImport (ExtImport)

data Db = Db
  { system :: Maybe DbSystem,
    seeds :: Maybe [ExtImport],
    prisma :: Maybe PrismaOptions
  }
  deriving (Show, Eq, Data)

data DbSystem = PostgreSQL | SQLite
  deriving (Show, Eq, Data)

data PrismaOptions = Prisma
  { clientPreviewFeatures :: Maybe [String]
  }
  deriving (Show, Eq, Data)
