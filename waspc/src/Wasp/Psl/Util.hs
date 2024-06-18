module Wasp.Psl.Util where

import Data.Foldable (find)
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Model as Psl.Model

findIdField :: Psl.Model.Body -> Maybe Psl.Model.Field
findIdField (Psl.Model.Body elements) = find isIdField fields
  where
    fields = [field | (Psl.Model.ElementField field) <- elements]

    isIdField :: Psl.Model.Field -> Bool
    isIdField Psl.Model.Field {_attrs = attrs} = any (\attr -> Psl.Attribute._attrName attr == attrNameAssociatedWitIdField) attrs

    -- We define an ID field as a field that has the @id attribute.
    attrNameAssociatedWitIdField :: String
    attrNameAssociatedWitIdField = "id"

findIdBlockAttribute :: Psl.Model.Body -> Maybe Psl.Attribute.Attribute
findIdBlockAttribute (Psl.Model.Body elements) = find isIdBlockAttribute attributes
  where
    attributes = [attr | (Psl.Model.ElementBlockAttribute attr) <- elements]

    isIdBlockAttribute :: Psl.Attribute.Attribute -> Bool
    isIdBlockAttribute Psl.Attribute.Attribute {_attrName = attrName} = attrName == idBlockAttributeName

    -- We define the ID block attribute as an attribute with the name @@id.
    idBlockAttributeName :: String
    idBlockAttributeName = "id"

doesPslFieldHaveAttribute :: String -> Psl.Model.Field -> Bool
doesPslFieldHaveAttribute name Psl.Model.Field {_attrs = attrs} = any ((== name) . Psl.Attribute._attrName) attrs

findPrismaConfigBlockKeyValuePair :: String -> [Psl.ConfigBlock.KeyValuePair] -> Maybe String
findPrismaConfigBlockKeyValuePair searchKey =
  fmap (\(Psl.ConfigBlock.KeyValuePair _ value) -> value)
    . find (\(Psl.ConfigBlock.KeyValuePair key _) -> key == searchKey)
