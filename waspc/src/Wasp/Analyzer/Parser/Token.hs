module Wasp.Analyzer.Parser.Token where

import Wasp.Analyzer.Parser.SourcePosition (SourcePosition, calcNextPosition)

data TokenType
  = TSpecialChar SpecialChar
  | TKeyword Keyword
  | TString String
  | TInt Integer
  | TDouble Double
  | TLQuote String
  | TQuoted String
  | TRQuote String
  | TIdentifier String
  | TEOF
  deriving (Eq, Show)

data SpecialChar
  = LParen
  | RParen
  | LSquare
  | RSquare
  | LCurly
  | RCurly
  | Comma
  | Colon
  deriving (Eq, Show)

data Keyword
  = Import
  | From
  | WaspTrue -- avoids conflicts with Prelude.True and Prelude.False
  | WaspFalse
  deriving (Eq, Show, Enum, Bounded)

prettyShowKeyword :: Keyword -> String
prettyShowKeyword keyword = case keyword of
  Import -> "import"
  From -> "from"
  WaspFalse -> "false"
  WaspTrue -> "true"

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
