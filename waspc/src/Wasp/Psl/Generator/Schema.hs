module Wasp.Psl.Generator.Schema
  ( generateSchemaElement,
    generateModelBody,
  )
where

import Data.List (intercalate)
import qualified Wasp.Psl.Ast.Schema as Ast

generateSchemaElement :: Ast.SchemaElement -> String
generateSchemaElement (Ast.SchemaModel (Ast.Model name body)) = "model " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
generateSchemaElement (Ast.SchemaEnum (Ast.PrismaEnum name values)) = "enum " ++ name ++ " {\n" ++ generateEnumBody values ++ "}"
generateSchemaElement (Ast.SchemaDatasource (Ast.Datasource name content)) = "datasource " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"
generateSchemaElement (Ast.SchemaGenerator (Ast.Generator name content)) = "generator " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"

generateConfigBlockKeyValues :: [Ast.ConfigBlockKeyValue] -> String
generateConfigBlockKeyValues keyValues = unlines $ generateConfigBlockKeyValue <$> keyValues
  where
    generateConfigBlockKeyValue (Ast.ConfigBlockKeyValue key value) = key ++ " = " ++ value

generateEnumBody :: [Ast.EnumField] -> String
generateEnumBody values = unlines $ map ("  " ++) $ generateEnumField <$> values

generateEnumField :: Ast.EnumField -> String
generateEnumField (Ast.EnumValue value attrs) =
  value ++ concatMap ((" " ++) . generateAttribute) attrs
generateEnumField (Ast.EnumBlockAttribute attribute) = "@" ++ generateAttribute attribute

generateModelBody :: Ast.Body -> String
generateModelBody (Ast.Body elements) = unlines $ map (("  " ++) . generateModelElement) elements

generateModelElement :: Ast.Element -> String
generateModelElement (Ast.ElementField field) =
  Ast._name field ++ " "
    ++ generateModelFieldType (Ast._type field)
    ++ concatMap generateModelFieldTypeModifier (Ast._typeModifiers field)
    ++ concatMap ((" " ++) . generateAttribute) (Ast._attrs field)
generateModelElement (Ast.ElementBlockAttribute attribute) =
  "@" ++ generateAttribute attribute

generateModelFieldType :: Ast.FieldType -> String
generateModelFieldType fieldType = case fieldType of
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

generateModelFieldTypeModifier :: Ast.FieldTypeModifier -> String
generateModelFieldTypeModifier typeModifier = case typeModifier of
  -- We validate the unsupported optional list in the AppSpec validator so it's okay if we decide to handle it here.
  -- It helps us with writing unit tests for the generator.
  Ast.UnsupportedOptionalList -> "[]?"
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
