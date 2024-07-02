module Wasp.Psl.Parser.Attribute
  ( attribute,
    blockAttribute,
  )
where

import Data.Maybe (fromMaybe)
import Text.Parsec
  ( char,
    optionMaybe,
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import Wasp.Psl.Parser.Argument
  ( argumentList,
    identifierStrParser,
  )

attribute :: Parser Psl.Attribute.Attribute
attribute = do
  _ <- char '@'

  name <- identifierStrParser

  maybeArgs <- optionMaybe argumentList
  return $
    Psl.Attribute.Attribute
      { Psl.Attribute._attrName = name,
        Psl.Attribute._attrArgs = fromMaybe [] maybeArgs
      }

blockAttribute :: Parser Psl.Attribute.Attribute
blockAttribute = char '@' >> attribute
