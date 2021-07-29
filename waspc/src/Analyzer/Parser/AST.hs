module Analyzer.Parser.AST where

type Identifier = String

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
  | Identifier Identifier
  | Quoter Identifier String
  deriving (Eq, Show)

data ExtImportName
  = -- | Represents external imports like @import Identifier from "file.js"@
    ExtImportModule Identifier
  | -- | Represents external imports like @import { Identifier } from "file.js"@
    ExtImportField Identifier
  deriving (Eq, Show)
