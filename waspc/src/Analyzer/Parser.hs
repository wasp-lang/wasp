module Analyzer.Parser
  ( AST (..)
  , Stmt (..)
  , Expr (..)
  , ExtImportName (..)
  , Ident
  , ParseError
  , parse
  ) where

type Ident = String

newtype AST = AST { astStmts :: [Stmt] }

data Stmt = Decl Ident Ident Expr

data Expr = Dict [(Ident, Expr)]
          | List [Expr]
          | StringLiteral String
          | IntegerLiteral Integer
          | DoubleLiteral Double
          | ExtImport ExtImportName String
          | Var Ident
          | Quoter Ident String Ident

data ExtImportName = ExtImportModule Ident | ExtImportField Ident

data ParseError

parse :: String -> Either ParseError AST
parse _ = Right $ AST { astStmts = [] }
