module Analyzer.Parser.Token where

-- | The first character on the first line is at position @Position 1 1@
data SourcePosition = SourcePosition Int Int deriving (Eq, Show)

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
  | TLQuote String
  | TRQuote String
  | TQuoted String
  | TIdentifier String
  | TEOF
  deriving (Eq, Show)

data Token = Token
  { tokenClass :: TokenClass,
    tokenPosition :: SourcePosition,
    tokenLexeme :: String
  }
  deriving (Eq, Show)
