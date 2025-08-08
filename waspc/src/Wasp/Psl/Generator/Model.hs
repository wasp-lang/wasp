module Wasp.Psl.Generator.Model
  ( generateModelBody,
  )
where

import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Generator.Attribute (generateAttribute)
import Wasp.Psl.Generator.Common (PslSource)
import Wasp.Psl.Generator.WithCtx (generateWithCtx)

generateModelBody :: Psl.Model.Body -> PslSource
generateModelBody (Psl.Model.Body elements) = unlines $ generateWithCtx generateModelElement <$> elements

generateModelElement :: Psl.Model.Element -> PslSource
generateModelElement (Psl.Model.ElementField field) =
  Psl.Model._name field ++ " "
    ++ generateModelFieldType (Psl.Model._type field)
    ++ concatMap generateModelFieldTypeModifier (Psl.Model._typeModifiers field)
    ++ concatMap ((" " ++) . generateAttribute) (Psl.Model._attrs field)
generateModelElement (Psl.Model.ElementBlockAttribute attribute) =
  "@" ++ generateAttribute attribute

generateModelFieldType :: Psl.Model.FieldType -> PslSource
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

generateModelFieldTypeModifier :: Psl.Model.FieldTypeModifier -> PslSource
generateModelFieldTypeModifier typeModifier = case typeModifier of
  -- We validate the unsupported optional list in the AppSpec validator so it's okay if we decide to handle it here.
  -- It helps us with writing unit tests for the generator.
  Psl.Model.List -> "[]"
  Psl.Model.Optional -> "?"
