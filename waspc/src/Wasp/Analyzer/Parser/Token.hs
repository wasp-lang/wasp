module Wasp.Analyzer.Parser.Token where

import Wasp.Analyzer.Parser.SourcePosition (SourcePosition, calcNextPosition)

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
    tokenStartPosition :: SourcePosition,
    tokenLexeme :: String
  }
  deriving (Eq, Show)

-- | Calculates source position of the last character in the token lexeme.
calcTokenEndPos :: Token -> SourcePosition
calcTokenEndPos (Token _ startPos "") = startPos
calcTokenEndPos t = calcNextPosition (init $ tokenLexeme t) (tokenStartPosition t)
