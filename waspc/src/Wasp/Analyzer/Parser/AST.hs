module Wasp.Analyzer.Parser.AST
  ( AST (..),
    Stmt (..),
    Expr (..),
    Identifier,
    ExtImportName (..),
  )
where

import Wasp.AppSpec.ExtImport (ExtImportName (..))

newtype AST = AST {astStmts :: [Stmt]} deriving (Eq, Show)

-- Decl <declType> <name> <body>
data Stmt = Decl Identifier Identifier Expr deriving (Eq, Show)

data Expr
  = Dict [(Identifier, Expr)]
  | List [Expr]
  | StringLiteral String
  | IntegerLiteral Integer
  | DoubleLiteral Double
  | BoolLiteral Bool
  | ExtImport ExtImportName String
  | Var Identifier
  | Quoter Identifier String
  deriving (Eq, Show)

type Identifier = String
