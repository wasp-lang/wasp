module Wasp.Psl.Util where

import Data.Foldable (find)
import qualified Wasp.Psl.Ast.Model as PslModel

findPrimaryKeyField :: PslModel.Body -> Maybe PslModel.Field
findPrimaryKeyField (PslModel.Body elements) = find isIdField fields
  where
    fields = [field | (PslModel.ElementField field) <- elements]

    isIdField :: PslModel.Field -> Bool
    isIdField PslModel.Field {_attrs = attrs} = any (\attr -> PslModel._attrName attr == attrNameAssociatedWithPrimaryKeyField) attrs

    -- We define a primary field as a field that has the @id attribute.
    attrNameAssociatedWithPrimaryKeyField :: String
    attrNameAssociatedWithPrimaryKeyField = "id"
