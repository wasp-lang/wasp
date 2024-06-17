module Wasp.Psl.Generator.Attribute
  ( generateAttribute,
  )
where

import Data.List (intercalate)
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute

generateAttribute :: Psl.Attribute.Attribute -> String
generateAttribute attribute =
  "@" ++ Psl.Attribute._attrName attribute
    ++ if null (Psl.Attribute._attrArgs attribute)
      then ""
      else "(" ++ intercalate ", " (map generateAttributeArg (Psl.Attribute._attrArgs attribute)) ++ ")"

generateAttributeArg :: Psl.Attribute.AttributeArg -> String
generateAttributeArg (Psl.Attribute.AttrArgNamed name value) = name ++ ": " ++ generateAttrArgValue value
generateAttributeArg (Psl.Attribute.AttrArgUnnamed value) = generateAttrArgValue value

generateAttrArgValue :: Psl.Attribute.AttrArgValue -> String
generateAttrArgValue value = case value of
  Psl.Attribute.AttrArgString strValue -> show strValue
  Psl.Attribute.AttrArgIdentifier identifier -> identifier
  Psl.Attribute.AttrArgFunc funcName -> funcName ++ "()"
  Psl.Attribute.AttrArgFieldRefList refs -> "[" ++ intercalate ", " refs ++ "]"
  Psl.Attribute.AttrArgNumber numberStr -> numberStr
  Psl.Attribute.AttrArgUnknown unknownStr -> unknownStr

-- TODO: I should make sure to skip attributes that are not known in prisma.
--   Or maybe it would be better if that was done in previous step, where
--   we basically edit the AST by kicking out those attributes.
