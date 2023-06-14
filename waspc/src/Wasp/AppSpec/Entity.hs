{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity
  ( makeEntity,
    Entity,
    getFields,
    getPslModelBody,
    getIdField,
    getIdBlockAttribute,
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Entity.Field (Field)
import qualified Wasp.AppSpec.Entity.Field as Field
import qualified Wasp.Psl.Ast.Model as PslModel
import Wasp.Psl.Util (findIdBlockAttribute, findIdField)

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

getIdField :: Entity -> Maybe PslModel.Field
getIdField = findIdField . getPslModelBody

getIdBlockAttribute :: Entity -> Maybe PslModel.Attribute
getIdBlockAttribute = findIdBlockAttribute . getPslModelBody
