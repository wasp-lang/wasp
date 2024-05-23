module Wasp.Psl.Parser.Enum
  ( enum,
  )
where

import Text.Parsec
  ( many1,
  )
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Model as Model
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
enum :: Parser Model.SchemaElement
enum = do
  whiteSpace
  _ <- reserved "enum"
  enumName <- identifier
  values <- braces (many1 identifier)
  return $ Model.SchemaEnum $ Model.PrismaEnum enumName values
