module Wasp.Psl.Parser.Type
  ( typeBlock,
  )
where

import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Type as Psl.Type
import Wasp.Psl.Parser.Common
  ( braces,
    identifier,
    reserved,
  )
import Wasp.Psl.Parser.Model (body)

-- | Parses PSL (Prisma Schema Language) type.
-- Example of PSL type:
--   type Photo {
--     height Int    @default(200)
--     width  Int    @default(100)
--     url    String
--   }
--
-- From the Prisma docs:
-- > Composite types are only available with MongoDB.
-- > Composite types, known as embedded documents in MongoDB,
-- > allow you to embed records within other records.
--
-- Even though Wasp doesn't yet support MongoDB, we wanted to
-- cover all the features of Prisma in the PSL.
typeBlock :: Parser Psl.Type.Type
typeBlock = do
  reserved "type"
  typeName <- identifier
  Psl.Type.Type typeName <$> braces body
