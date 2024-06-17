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
import Wasp.Psl.Parser.Attribute (attribute, blockAttribute)
import Wasp.Psl.Parser.Common
  ( braces,
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
enum :: Parser Psl.Schema.Block
enum = do
  whiteSpace
  reserved "enum"
  enumName <- identifier
  values <- braces (many1 enumField)
  return $ Psl.Schema.EnumBlock $ Psl.Enum.Enum enumName values

enumField :: Parser Psl.Enum.Element
enumField =
  try enumValue
    <|> try enumBlockAttribute

enumValue :: Parser Psl.Enum.Element
enumValue = Psl.Enum.ElementValue <$> identifier <*> many (try attribute)

enumBlockAttribute :: Parser Psl.Enum.Element
enumBlockAttribute = Psl.Enum.ElementBlockAttribute <$> blockAttribute
