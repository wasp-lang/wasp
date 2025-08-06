module Wasp.Psl.Parser.Type
  ( typeBlock,
  )
where

import qualified Wasp.Psl.Ast.Type as Psl.Type
import Wasp.Psl.Parser.Common
  ( Parser,
    braces,
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
-- PSL type blocks have the same syntax as
-- Prisma model blocks, but they are prefixed with
-- `type` keyword. That's why we are reusing the
-- `body` parser from `Model` to parse the body
-- of the type block.
typeBlock :: Parser Psl.Type.Type
typeBlock = do
  reserved "type"
  typeName <- identifier
  Psl.Type.Type typeName <$> braces body
