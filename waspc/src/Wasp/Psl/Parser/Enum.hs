module Wasp.Psl.Parser.Enum
  ( enum,
  )
where

import Text.Megaparsec (choice, many, some, try)
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import Wasp.Psl.Parser.Attribute (attribute, blockAttribute)
import Wasp.Psl.Parser.Common
  ( Parser,
    braces,
    identifier,
    reserved,
  )
import Wasp.Psl.Parser.WithCtx (withCtx)

-- | Parses PSL (Prisma Schema Language enum).
-- Example of PSL enum:
--   enum Role {
--     USER @map("user")
--     ADMIN
--     @@map("role")
--   }
enum :: Parser Psl.Enum.Enum
enum = do
  reserved "enum"
  enumName <- identifier
  Psl.Enum.Enum enumName <$> braces (some $ withCtx enumField)

enumField :: Parser Psl.Enum.Element
enumField =
  choice
    [ try enumValue,
      try enumBlockAttribute
    ]

enumValue :: Parser Psl.Enum.Element
enumValue = Psl.Enum.ElementValue <$> identifier <*> many (try attribute)

enumBlockAttribute :: Parser Psl.Enum.Element
enumBlockAttribute = Psl.Enum.ElementBlockAttribute <$> blockAttribute
