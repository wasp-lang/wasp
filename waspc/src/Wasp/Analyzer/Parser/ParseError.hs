module Wasp.Analyzer.Parser.ParseError where

import Wasp.Analyzer.Parser.Token

data ParseError
  = -- | A lexical error representing an invalid character
    UnexpectedChar Char SourcePosition
  | -- | A parse error caused by some token
    ParseError Token
  | QuoterDifferentTags (String, SourcePosition) (String, SourcePosition)
  deriving (Eq, Show)
