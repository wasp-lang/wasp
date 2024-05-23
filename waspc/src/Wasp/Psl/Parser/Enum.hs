module Wasp.Psl.Parser.Enum
  ( enum,
  )
where

import Text.Parsec
  ( many1,
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import Wasp.Psl.Parser.Common
  ( braces,
    identifier,
    reserved,
    whiteSpace,
  )

-- | Parses PSL (Prisma Schema Language enum).
-- Example of PSL enum:
--   enum Role {
--     USER
--     ADMIN
--   }
enum :: Parser Psl.Ast.SchemaElement
enum = do
  whiteSpace
  reserved "enum"
  enumName <- identifier
  values <- braces (many1 identifier)
  return $ Psl.Ast.SchemaEnum $ Psl.Ast.PrismaEnum enumName values
