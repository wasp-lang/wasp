module Wasp.Psl.Util where

import Data.Foldable (find)
import qualified Wasp.Psl.Ast.Model as PslModel

findIdField :: PslModel.Body -> Maybe PslModel.Field
findIdField (PslModel.Body elements) = find isIdField fields
  where
    fields = [field | (PslModel.ElementField field) <- elements]

    isIdField :: PslModel.Field -> Bool
    isIdField PslModel.Field {_attrs = attrs} = any (\attr -> PslModel._attrName attr == attrNameAssociatedWitIdField) attrs

    -- We define an ID field as a field that has the @id attribute.
    attrNameAssociatedWitIdField :: String
    attrNameAssociatedWitIdField = "id"

findIdBlockAttribute :: PslModel.Body -> Maybe PslModel.Attribute
findIdBlockAttribute (PslModel.Body elements) = find isIdBlockAttribute attributes
  where
    attributes = [attr | (PslModel.ElementBlockAttribute attr) <- elements]

    isIdBlockAttribute :: PslModel.Attribute -> Bool
    isIdBlockAttribute PslModel.Attribute {_attrName = attrName} = attrName == idBlockAttributeName

    -- We define the ID block attribute as an attribute with the name @@id.
    idBlockAttributeName :: String
    idBlockAttributeName = "id"

doesPslFieldHaveAttribute :: String -> PslModel.Field -> Bool
doesPslFieldHaveAttribute name PslModel.Field {_attrs = attrs} = any ((== name) . PslModel._attrName) attrs
