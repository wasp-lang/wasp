module Wasp.Analyzer.Parser.Token where

import Wasp.Analyzer.Parser.SourcePosition (SourcePosition)

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
