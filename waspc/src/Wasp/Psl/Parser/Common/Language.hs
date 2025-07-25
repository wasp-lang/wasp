-- This module is a copy-paste of the `empyDef` from `Text.Parsec.Language` module, with the
-- following changes:
-- 1. Made `commentLine` a `Parser` instead of a `String`.
-- This way, we can accept arbitrary parsers for line comments, which is useful for our use case, as
-- just adding `//` as a line comment would make the parser ignore documentation comments `///`.
-- Original at
-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/src/Text.Parsec.Language.html#emptyDef
-- -------------------------------------------------------------------------------------------------

-- |
-- Module      :  Text.Parsec.Language
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (uses non-portable module Text.Parsec.Token)
--
-- A helper module that defines some language definitions that can be used
-- to instantiate a token parser (see "Text.Parsec.Token").
module Wasp.Psl.Parser.Common.Language
  ( emptyDef,
  )
where

import Control.Monad (void)
import Text.Parsec
import Wasp.Psl.Parser.Common.Token

-----------------------------------------------------------
-- minimal language definition
--------------------------------------------------------

-- | This is the most minimal token definition. It is recommended to use
-- this definition as the basis for other definitions. @emptyDef@ has
-- no reserved names or operators, is case sensitive and doesn't accept
-- comments, identifiers or operators.
emptyDef :: LanguageDef st
emptyDef =
  LanguageDef
    { commentLine = void $ string "//",
      nestedComments = True,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> oneOf "_'",
      opStart = opLetter emptyDef,
      opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      reservedOpNames = [],
      reservedNames = [],
      caseSensitive = True
    }
