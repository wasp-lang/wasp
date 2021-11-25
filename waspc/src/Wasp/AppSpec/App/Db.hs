{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Db
  ( Db (..),
    DbSystem (..),
  )
where

import Data.Data (Data)

data Db = Db
  { system :: Maybe DbSystem
  }
  deriving (Show, Eq, Data)

data DbSystem = PostgreSQL | SQLite
  deriving (Show, Eq, Data)
