module Wasp.Psl.Parser.View
  ( view,
  )
where

import qualified Wasp.Psl.Ast.View as Psl.View
import Wasp.Psl.Parser.Common
  ( Parser,
  )
import Wasp.Psl.Parser.Model (body)
import Wasp.Psl.Parser.Tokens
  ( braces,
    identifier,
    reserved,
  )

-- | Parses PSL (Prisma Schema Language) view.
-- Example of PSL view:
--   view User {
--     id Int @id
--     name String
--   }
--
-- PSL view blocks have the same syntax as
-- Prisma model blocks, but they are prefixed with
-- `view` keyword. That's why we are reusing the
-- `body` parser from `Model` to parse the body
-- of the type block.
view :: Parser Psl.View.View
view = do
  reserved "view"
  viewName <- identifier
  Psl.View.View viewName <$> braces body
