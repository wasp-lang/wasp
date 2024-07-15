module Wasp.Psl.Parser.View
  ( view,
  )
where

import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.View as Psl.View
import Wasp.Psl.Parser.Common
  ( braces,
    identifier,
    reserved,
  )
import Wasp.Psl.Parser.Model (body)

-- | Parses PSL (Prisma Schema Language) view.
-- Example of PSL view:
--   view User {
--     id Int @id
--     name String
--   }
view :: Parser Psl.View.View
view = do
  reserved "view"
  viewName <- identifier
  Psl.View.View viewName <$> braces body
