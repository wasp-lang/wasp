module Analyzer.Syntax
  ( Token (..)
  , AST (..)
  , Stmt (..)
  , Expr (..)
  , ExtImportName (..)
  , Ident
  , ParseError (..)
  , Posn (..)
  ) where

-- TOKEN TYPES

data Token = TLCurly
           | TRCurly
           | TComma
           | TColon
           | TLSquare
           | TRSquare
           | TImport
           | TFrom
           | TString String
           | TInt Integer
           | TDouble Double
           | TTrue
           | TFalse
           | TQuoter (String, String, String)
           | TIdent String
           | TEOF
           deriving (Eq, Show)

-- AST TYPES

type Ident = String

newtype AST = AST { astStmts :: [Stmt] } deriving (Eq, Show)

data Stmt = Decl Ident Ident Expr deriving (Eq, Show)

data Expr = Dict [(Ident, Expr)]
          | List [Expr]
          | StringLiteral String
          | IntegerLiteral Integer
          | DoubleLiteral Double
          | BoolLiteral Bool
          | ExtImport ExtImportName String
          | Var Ident
          | Quoter Ident String Ident deriving (Eq, Show)

data ExtImportName = ExtImportModule Ident | ExtImportField Ident deriving (Eq, Show)

-- ERROR TYPES

data Posn = Posn { line :: Int, col :: Int } deriving (Eq, Show)

data ParseError = UnexpectedChar Char Posn
                | ParseError Token Posn
                  deriving (Eq, Show)
