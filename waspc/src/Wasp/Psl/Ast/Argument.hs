{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Argument where

import Data.Data (Data)

data Argument
  = ArgNamed String Expression
  | ArgUnnamed Expression
  deriving (Show, Eq, Data)

data Expression
  = StringExpr String
  | -- | IdentifierExpr are defined in the Prisma grammar (under the name "path")
    -- as a sequence of identifiers separated by dots.
    -- "name", "db.Something" or "db.SomethingElse.Extra" are all valid IdentifierExprs.
    IdentifierExpr String
  | FuncExpr String [Argument]
  | ArrayExpr [Expression]
  | NumberExpr String
  deriving (Show, Eq, Data)
