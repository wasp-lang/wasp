module Wasp.Psl.Generator.Schema
  ( generateSchemaElement,
    generateModelBody,
  )
where

import Data.List (intercalate)
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema

generateSchemaElement :: Psl.Schema.SchemaElement -> String
generateSchemaElement (Psl.Schema.SchemaModel (Psl.Model.Model name body)) = "model " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
generateSchemaElement (Psl.Schema.SchemaEnum (Psl.Enum.Enum name values)) = "enum " ++ name ++ " {\n" ++ generateEnumBody values ++ "}"
generateSchemaElement (Psl.Schema.SchemaDatasource (Psl.ConfigBlock.Datasource name content)) = "datasource " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"
generateSchemaElement (Psl.Schema.SchemaGenerator (Psl.ConfigBlock.Generator name content)) = "generator " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"

generateConfigBlockKeyValues :: [Psl.ConfigBlock.ConfigBlockKeyValue] -> String
generateConfigBlockKeyValues keyValues = unlines $ generateConfigBlockKeyValue <$> keyValues
  where
    generateConfigBlockKeyValue (Psl.ConfigBlock.ConfigBlockKeyValue key value) = key ++ " = " ++ value

generateEnumBody :: [Psl.Enum.EnumElement] -> String
generateEnumBody values = unlines $ map ("  " ++) $ generateEnumField <$> values

generateEnumField :: Psl.Enum.EnumElement -> String
generateEnumField (Psl.Enum.EnumElementValue value attrs) =
  value ++ concatMap ((" " ++) . generateAttribute) attrs
generateEnumField (Psl.Enum.EnumElementBlockAttribute attribute) = "@" ++ generateAttribute attribute

generateModelBody :: Psl.Model.ModelBody -> String
generateModelBody (Psl.Model.ModelBody elements) = unlines $ map (("  " ++) . generateModelElement) elements

generateModelElement :: Psl.Model.ModelElement -> String
generateModelElement (Psl.Model.ModelElementField field) =
  Psl.Model._name field ++ " "
    ++ generateModelFieldType (Psl.Model._type field)
    ++ concatMap generateModelFieldTypeModifier (Psl.Model._typeModifiers field)
    ++ concatMap ((" " ++) . generateAttribute) (Psl.Model._attrs field)
generateModelElement (Psl.Model.ModelElementBlockAttribute attribute) =
  "@" ++ generateAttribute attribute

generateModelFieldType :: Psl.Model.ModelFieldType -> String
generateModelFieldType fieldType = case fieldType of
  Psl.Model.String -> "String"
  Psl.Model.Boolean -> "Boolean"
  Psl.Model.Int -> "Int"
  Psl.Model.BigInt -> "BigInt"
  Psl.Model.Float -> "Float"
  Psl.Model.Decimal -> "Decimal"
  Psl.Model.DateTime -> "DateTime"
  Psl.Model.Json -> "Json"
  Psl.Model.Bytes -> "Bytes"
  Psl.Model.UserType label -> label
  Psl.Model.Unsupported typeName -> "Unsupported(" ++ show typeName ++ ")"

generateModelFieldTypeModifier :: Psl.Model.ModelFieldTypeModifier -> String
generateModelFieldTypeModifier typeModifier = case typeModifier of
  -- We validate the unsupported optional list in the AppSpec validator so it's okay if we decide to handle it here.
  -- It helps us with writing unit tests for the generator.
  Psl.Model.UnsupportedOptionalList -> "[]?"
  Psl.Model.List -> "[]"
  Psl.Model.Optional -> "?"

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
