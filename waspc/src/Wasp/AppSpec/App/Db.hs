{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Db
  ( Db (..),
    DbSystem (..),
    PrismaOptions (..),
    PrismaDbExtension (..),
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

data PrismaOptions = PrismaOptions
  { clientPreviewFeatures :: Maybe [String],
    dbExtensions :: Maybe [PrismaDbExtension]
  }
  deriving (Show, Eq, Data)

data PrismaDbExtension = PrismaDbExtension
  { name :: String,
    map :: Maybe String,
    version :: Maybe String,
    schema :: Maybe String
  }
  deriving (Show, Eq, Data)
