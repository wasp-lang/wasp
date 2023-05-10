{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity
  ( makeEntity,
    Entity,
    getFields,
    getPslModelBody,
    getPrimaryField,
  )
where

import Data.Data (Data)
import Data.Foldable (find)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Entity.Field (Field)
import qualified Wasp.AppSpec.Entity.Field as Field
import qualified Wasp.Psl.Ast.Model as PslModel

data Entity = Entity
  { fields :: ![Field],
    pslModelBody :: !PslModel.Body
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

makeEntity :: PslModel.Body -> Entity
makeEntity body =
  Entity
    { fields = makeEntityFieldsFromPslModelBody body,
      pslModelBody = body
    }
  where
    makeEntityFieldsFromPslModelBody :: PslModel.Body -> [Field]
    makeEntityFieldsFromPslModelBody (PslModel.Body pslElements) =
      Field.pslModelFieldToEntityField <$> [field | (PslModel.ElementField field) <- pslElements]

getFields :: Entity -> [Field]
getFields = fields

getPslModelBody :: Entity -> PslModel.Body
getPslModelBody = pslModelBody

getPrimaryField :: Entity -> Maybe PslModel.Field
getPrimaryField entity = find ((attrNameAssociatedWithPrimaryField `elem`) . getFieldAttrNames) pslFields
  where
    pslFields = [field | (PslModel.ElementField field) <- pslElements]
    (PslModel.Body pslElements) = getPslModelBody entity

    getFieldAttrNames :: PslModel.Field -> [String]
    getFieldAttrNames PslModel.Field {_attrs = attrs} = map PslModel._attrName attrs

    -- We define a primary field as a field that has the @id attribute.
    attrNameAssociatedWithPrimaryField :: String
    attrNameAssociatedWithPrimaryField = "id"
