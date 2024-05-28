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
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
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
enum :: Parser Psl.Ast.SchemaElement
enum = do
  whiteSpace
  reserved "enum"
  enumName <- identifier
  values <- braces (many1 enumField)
  return $ Psl.Ast.SchemaEnum $ Psl.Ast.PrismaEnum enumName values

enumField :: Parser Psl.Ast.EnumField
enumField =
  try enumValue
    <|> try enumBlockAttribute

enumValue :: Parser Psl.Ast.EnumField
enumValue = Psl.Ast.EnumValue <$> identifier <*> many (try fieldAttribute)

enumBlockAttribute :: Parser Psl.Ast.EnumField
enumBlockAttribute = Psl.Ast.EnumBlockAttribute <$> blockAttribute
