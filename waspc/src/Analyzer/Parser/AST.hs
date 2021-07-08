module Analyzer.Parser.AST where

type Ident = String

newtype AST = AST {astStmts :: [Stmt]} deriving (Eq, Show)

data Stmt = Decl Ident Ident Expr deriving (Eq, Show)

data Expr
  = Dict [(Ident, Expr)]
  | List [Expr]
  | StringLiteral String
  | IntegerLiteral Integer
  | DoubleLiteral Double
  | BoolLiteral Bool
  | ExtImport ExtImportName String
  | Identifier Ident
  | Quoter Ident String
  deriving (Eq, Show)

data ExtImportName
  = -- | Represents external imports like @import Ident from "file.js"@
    ExtImportModule Ident
  | -- | Represents external imports like @import { Ident } from "file.js"@
    ExtImportField Ident
  deriving (Eq, Show)
