module Wasp.Analyzer.Parser.Token where

-- | The first character on the first line is at position @Position 1 1@
data SourcePosition = SourcePosition Int Int deriving (Eq, Show)

data TokenType
  = TLParen
  | TRParen
  | TLSquare
  | TRSquare
  | TLCurly
  | TRCurly
  | TComma
  | TColon
  | TImport
  | TFrom
  | TTrue
  | TFalse
  | TString String
  | TInt Integer
  | TDouble Double
  | TLQuote String
  | TQuoted String
  | TRQuote String
  | TIdentifier String
  | TEOF
  deriving (Eq, Show)

data Token = Token
  { tokenType :: TokenType,
    tokenPosition :: SourcePosition,
    tokenLexeme :: String
  }
  deriving (Eq, Show)
