module Wasp.Psl.Parser.Enum
  ( enum,
  )
where

import Text.Parsec
  ( many,
    many1,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Parser.Common
  ( blockAttribute,
    braces,
    fieldAttribute,
    identifier,
    reserved,
    whiteSpace,
  )

-- | Parses PSL (Prisma Schema Language enum).
-- Example of PSL enum:
--   enum Role {
--     USER @map("user")
--     ADMIN
--     @@map("role")
--   }
enum :: Parser Psl.Schema.SchemaElement
enum = do
  whiteSpace
  reserved "enum"
  enumName <- identifier
  values <- braces (many1 enumField)
  return $ Psl.Schema.SchemaEnum $ Psl.Enum.Enum enumName values

enumField :: Parser Psl.Enum.EnumElement
enumField =
  try enumValue
    <|> try enumBlockAttribute

enumValue :: Parser Psl.Enum.EnumElement
enumValue = Psl.Enum.EnumElementValue <$> identifier <*> many (try fieldAttribute)

enumBlockAttribute :: Parser Psl.Enum.EnumElement
enumBlockAttribute = Psl.Enum.EnumElementBlockAttribute <$> blockAttribute
