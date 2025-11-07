module Wasp.Psl.Generator.Attribute
  ( generateAttribute,
  )
where

import Data.List (intercalate)
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import Wasp.Psl.Generator.Argument (generateArgument)
import Wasp.Psl.Generator.Common (PslSource)

generateAttribute :: Psl.Attribute.Attribute -> PslSource
generateAttribute attribute =
  "@"
    ++ Psl.Attribute._attrName attribute
    ++ if null (Psl.Attribute._attrArgs attribute)
      then ""
      else "(" ++ intercalate ", " (map generateArgument (Psl.Attribute._attrArgs attribute)) ++ ")"

-- TODO: I should make sure to skip attributes that are not known in prisma.
--   Or maybe it would be better if that was done in previous step, where
--   we basically edit the AST by kicking out those attributes.
