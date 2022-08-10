{-# LANGUAGE DeriveGeneric #-}

module Wasp.Analyzer.Parser.Token
  ( Token (..),
    TokenKind (..),
    tokenKindIsTrivia,
    showTokenKind,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

-- | The kind of token
--
-- This makes no distinction between value-containing tokens and atomic tokens
-- (i.e. "String" and "KwImport") to make kind comparison easier. To recover the
-- value associated with a token, you need the context provided by a "Token".
data TokenKind
  = White
  | -- | Newlines (only \n) are separated from whitespace to accomodate line-break
    -- based error recovery, if that is ever implemented in the future
    Newline
  | Comment
  | LParen
  | RParen
  | LSquare
  | RSquare
  | LCurly
  | RCurly
  | Comma
  | Colon
  | KwImport
  | KwFrom
  | KwTrue
  | KwFalse
  | String
  | Int
  | Double
  | -- | "{= <identifier>"
    LQuote
  | -- | "<identifier> =}"
    RQuote
  | Quoted
  | -- | Non-keyword identifier
    Identifier
  | -- | Kind for unexpected characters
    Error
  deriving (Eq, Ord, Show, Generic)

instance NFData TokenKind

instance ToJSON TokenKind

-- | A token representing a span of text from the source.
--
-- __Note:__
--
-- The lexeme that a token represents is not stored with the token. This is
-- because the CST parser does not need to use lexemes during its parsing. The
-- effect of this is that we need to carry the source string around after parsing
-- and pass it to any functions that will need to get the lexeme from a token
-- or CST nodes. Needing to access a lexeme for a token is the edge case, since
-- for most tokens the lexeme doesn't matter. Some of the cases that need it are:
--
-- - Error reporting
-- - Getting values for literal nodes, variable names, and quoters
--
-- We could store the lexeme inside tokens and/or inside CST nodes. Two ways
-- we could go about storing lexemes in the CST nodes:
--
-- (1) We store a lexeme for every syntax node, which could lead towards
--    somewhat significant memory usage, since the lexeme for each token is
--    duplicated by all of its ancestors.
-- (2) We store a lexeme only for syntax nodes with no children, which makes it
--     kind of complicated to get the lexeme for a non-leaf node.
data Token = Token
  { tokenKind :: !TokenKind,
    -- | The width of the text representing this token. The source position is
    -- not stored; only the width. Later, offsets into the source is computed
    -- from an entire tree of tokens, and source position is determined on
    -- demand (for example, when an offset is going to be displayerd in an error
    -- message).
    tokenWidth :: !Int
  }
  deriving (Eq, Show, Ord, Generic)

instance NFData Token

instance ToJSON Token

-- | Check if a "TokenKind" is trivia (a token kind that does not affect the
-- parse structure, namely whitespace and comments)
tokenKindIsTrivia :: TokenKind -> Bool
tokenKindIsTrivia White = True
tokenKindIsTrivia Newline = True
tokenKindIsTrivia Comment = True
tokenKindIsTrivia _ = False

showTokenKind :: TokenKind -> String
showTokenKind White = "<whitespace>"
showTokenKind Newline = "\\n"
showTokenKind Comment = "<comment>"
showTokenKind LParen = "'('"
showTokenKind RParen = "')'"
showTokenKind LSquare = "'['"
showTokenKind RSquare = "']'"
showTokenKind LCurly = "'{'"
showTokenKind RCurly = "'}'"
showTokenKind Comma = "','"
showTokenKind Colon = "':'"
showTokenKind KwImport = "'import'"
showTokenKind KwFrom = "'from'"
showTokenKind KwTrue = "'true'"
showTokenKind KwFalse = "'false'"
showTokenKind String = "<string>"
showTokenKind Int = "<number>"
showTokenKind Double = "<number>"
showTokenKind LQuote = "'{='"
showTokenKind RQuote = "'=}'"
showTokenKind Quoted = "<any>"
showTokenKind Identifier = "<identifier>"
showTokenKind Error = "<error>"
