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

import Data.Aeson (FromJSON (parseJSON))
import Data.Data (Data)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Entity.Field (Field)
import qualified Wasp.AppSpec.Entity.Field as Field
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Psl.Util (findIdBlockAttribute, findIdField)

data Entity = Entity
  { fields :: ![Field],
    pslModelBody :: !Psl.Model.Body
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

instance FromJSON Entity where
  parseJSON = const $ fail "Entity declarations in wasp are deprecated, entities are now defined via prisma.schema file."

makeEntity :: Psl.Model.Body -> Entity
makeEntity body =
  Entity
    { fields = makeEntityFieldsFromPslBody body,
      pslModelBody = body
    }
  where
    makeEntityFieldsFromPslBody :: Psl.Model.Body -> [Field]
    makeEntityFieldsFromPslBody (Psl.Model.Body pslElements) =
      Field.pslFieldToEntityField
        <$> [ field
              | (Psl.Model.ElementField field) <- Psl.WithCtx.getNode <$> pslElements
            ]

getFields :: Entity -> [Field]
getFields = fields

getPslModelBody :: Entity -> Psl.Model.Body
getPslModelBody = pslModelBody

getIdField :: Entity -> Maybe Psl.Model.Field
getIdField = findIdField . getPslModelBody

getIdBlockAttribute :: Entity -> Maybe Psl.Attribute.Attribute
getIdBlockAttribute = findIdBlockAttribute . getPslModelBody
