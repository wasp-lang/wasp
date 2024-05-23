module Wasp.Psl.Util where

import Data.Foldable (find)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast

findIdField :: Psl.Ast.Body -> Maybe Psl.Ast.Field
findIdField (Psl.Ast.Body elements) = find isIdField fields
  where
    fields = [field | (Psl.Ast.ElementField field) <- elements]

    isIdField :: Psl.Ast.Field -> Bool
    isIdField Psl.Ast.Field {_attrs = attrs} = any (\attr -> Psl.Ast._attrName attr == attrNameAssociatedWitIdField) attrs

    -- We define an ID field as a field that has the @id attribute.
    attrNameAssociatedWitIdField :: String
    attrNameAssociatedWitIdField = "id"

findIdBlockAttribute :: Psl.Ast.Body -> Maybe Psl.Ast.Attribute
findIdBlockAttribute (Psl.Ast.Body elements) = find isIdBlockAttribute attributes
  where
    attributes = [attr | (Psl.Ast.ElementBlockAttribute attr) <- elements]

    isIdBlockAttribute :: Psl.Ast.Attribute -> Bool
    isIdBlockAttribute Psl.Ast.Attribute {_attrName = attrName} = attrName == idBlockAttributeName

    -- We define the ID block attribute as an attribute with the name @@id.
    idBlockAttributeName :: String
    idBlockAttributeName = "id"

doesPslFieldHaveAttribute :: String -> Psl.Ast.Field -> Bool
doesPslFieldHaveAttribute name Psl.Ast.Field {_attrs = attrs} = any ((== name) . Psl.Ast._attrName) attrs
