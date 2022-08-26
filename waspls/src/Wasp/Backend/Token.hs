{-# LANGUAGE DeriveGeneric #-}

module Wasp.Backend.Token
  ( Token (..),
    TokenKind (..),
    tokenKindIsTrivia,
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

-- | A token representing a span of text from the source. For value-containing
-- tokens, the value can be recovered using @tokenText@.
data Token = Token
  { tokenKind :: !TokenKind,
    -- | The width of the text representing this token. The source position is
    -- not stored; only the width. Later, offsets into the source is computed
    -- from an entire tree of tokens, and source position is determined on
    -- demand (for example, when an offset is going to be displayerd in an error
    -- message).
    --
    -- This is guaranteed to be equivalent to @length . tokenText@, but is
    -- stored explicitly since it is accessed frequently.
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
