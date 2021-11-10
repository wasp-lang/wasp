module Psl.Generator.Model
  ( generateModel,
  )
where

import Data.List (intercalate)
import qualified Psl.Ast.Model as Ast

generateModel :: Ast.Model -> String
generateModel (Ast.Model name body) = "model " ++ name ++ " {\n" ++ generateBody body ++ "\n}"

generateBody :: Ast.Body -> String
generateBody (Ast.Body elements) = unlines $ map (("  " ++) . generateElement) elements

generateElement :: Ast.Element -> String
generateElement (Ast.ElementField field) =
  Ast._name field ++ " "
    ++ generateFieldType (Ast._type field)
    ++ concatMap generateFieldTypeModifier (Ast._typeModifiers field)
    ++ concatMap ((" " ++) . generateAttribute) (Ast._attrs field)
generateElement (Ast.ElementBlockAttribute attribute) =
  "@" ++ generateAttribute attribute

generateFieldType :: Ast.FieldType -> String
generateFieldType fieldType = case fieldType of
  Ast.String -> "String"
  Ast.Boolean -> "Boolean"
  Ast.Int -> "Int"
  Ast.BigInt -> "BigInt"
  Ast.Float -> "Float"
  Ast.Decimal -> "Decimal"
  Ast.DateTime -> "DateTime"
  Ast.Json -> "Json"
  Ast.Bytes -> "Bytes"
  Ast.UserType label -> label
  Ast.Unsupported typeName -> "Unsupported(" ++ show typeName ++ ")"

generateFieldTypeModifier :: Ast.FieldTypeModifier -> String
generateFieldTypeModifier typeModifier = case typeModifier of
  Ast.List -> "[]"
  Ast.Optional -> "?"

generateAttribute :: Ast.Attribute -> String
generateAttribute attribute =
  "@" ++ Ast._attrName attribute
    ++ if null (Ast._attrArgs attribute)
      then ""
      else "(" ++ intercalate ", " (map generateAttributeArg (Ast._attrArgs attribute)) ++ ")"

generateAttributeArg :: Ast.AttributeArg -> String
generateAttributeArg (Ast.AttrArgNamed name value) = name ++ ": " ++ generateAttrArgValue value
generateAttributeArg (Ast.AttrArgUnnamed value) = generateAttrArgValue value

generateAttrArgValue :: Ast.AttrArgValue -> String
generateAttrArgValue value = case value of
  Ast.AttrArgString strValue -> show strValue
  Ast.AttrArgIdentifier identifier -> identifier
  Ast.AttrArgFunc funcName -> funcName ++ "()"
  Ast.AttrArgFieldRefList refs -> "[" ++ intercalate ", " refs ++ "]"
  Ast.AttrArgNumber numberStr -> numberStr
  Ast.AttrArgUnknown unknownStr -> unknownStr

-- TODO: I should make sure to skip attributes that are not known in prisma.
--   Or maybe it would be better if that was done in previous step, where
--   we basically edit the AST by kicking out those attributes.
