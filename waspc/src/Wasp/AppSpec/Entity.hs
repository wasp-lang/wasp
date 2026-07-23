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
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Util (findIdBlockAttribute, findIdField, getModelFields)

newtype Entity = Entity
  { pslModelBody :: Psl.Model.Body
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

instance FromJSON Entity where
  parseJSON = const $ fail "Entity declarations in wasp are deprecated, entities are now defined via prisma.schema file."

makeEntity :: Psl.Model.Body -> Entity
makeEntity = Entity

getFields :: Entity -> [Psl.Model.Field]
getFields = getModelFields . getPslModelBody

getPslModelBody :: Entity -> Psl.Model.Body
getPslModelBody = pslModelBody

getIdField :: Entity -> Maybe Psl.Model.Field
getIdField = findIdField . getPslModelBody

getIdBlockAttribute :: Entity -> Maybe Psl.Attribute.Attribute
getIdBlockAttribute = findIdBlockAttribute . getPslModelBody
