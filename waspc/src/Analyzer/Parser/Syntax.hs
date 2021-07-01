module Analyzer.Parser.Syntax
  ( Token (..),
    TokenClass (..),
    AST (..),
    Stmt (..),
    Expr (..),
    ExtImportName (..),
    Ident,
    ParseError (..),
    Posn (..),
  )
where

-- TOKEN TYPES

data TokenClass
  = TLCurly
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
  | -- | Open quote tag, contents, Close quote tag
    TQuoter (String, String, String)
  | TIdent String
  | TEOF
  deriving (Eq, Show)

data Token = Token
  { tokenClass :: TokenClass,
    tokenPosn :: Posn,
    tokenLexeme :: String
  }
  deriving (Eq, Show)

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
  | Quoter Ident String Ident
  deriving (Eq, Show)

data ExtImportName
  = -- | Represents external imports like @import Ident from "file.js"@
    ExtImportModule Ident
  | -- | Represents external imports like @import { Ident } from "file.js"@
    ExtImportField Ident
  deriving (Eq, Show)

-- | The first character on the first line is at position @Posn 1 1@
data Posn = Posn Int Int deriving (Eq, Show)

data ParseError
  = -- | A lexical error representing an invalid character
    UnexpectedChar Char Posn
  | -- | A parse error caused by some token
    ParseError Token
  deriving (Eq, Show)
