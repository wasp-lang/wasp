module Wasp.Psl.Parser.Attribute
  ( attribute,
    blockAttribute,
  )
where

import Data.Maybe (fromMaybe)
import Text.Megaparsec (optional)
import qualified Text.Megaparsec.Char as C
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import Wasp.Psl.Parser.Argument
  ( argumentList,
    identifierStrParser,
  )
import Wasp.Psl.Parser.Common (Parser)

attribute :: Parser Psl.Attribute.Attribute
attribute = do
  _ <- C.char '@'

  name <- identifierStrParser

  maybeArgs <- optional argumentList
  return $
    Psl.Attribute.Attribute
      { Psl.Attribute._attrName = name,
        Psl.Attribute._attrArgs = fromMaybe [] maybeArgs
      }

blockAttribute :: Parser Psl.Attribute.Attribute
blockAttribute = C.char '@' >> attribute
