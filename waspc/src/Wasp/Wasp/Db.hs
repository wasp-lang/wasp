module Wasp.Wasp.Db
  ( Db (..),
    DbSystem (..),
  )
where

newtype Db = Db
  { _system :: DbSystem
  }
  deriving (Show, Eq)

data DbSystem
  = PostgreSQL
  | SQLite
  deriving (Show, Eq)
