{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity
  ( makeEntity,
    Entity,
    getFields,
    getPslModelBody,
    getIdField,
    getIdBlockAttribute,
    isFieldUnique,
    -- only for testing:
    doesFieldHaveAttribute,
  )
where

import Data.Data (Data)
import Data.List (find)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Entity.Field (Field)
import qualified Wasp.AppSpec.Entity.Field as Field
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Util (doesPslFieldHaveAttribute, findIdBlockAttribute, findIdField)

data Entity = Entity
  { fields :: ![Field],
    pslModelBody :: !Psl.Model.Body
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

makeEntity :: Psl.Model.Body -> Entity
makeEntity body =
  Entity
    { fields = makeEntityFieldsFromPslBody body,
      pslModelBody = body
    }
  where
    makeEntityFieldsFromPslBody :: Psl.Model.Body -> [Field]
    makeEntityFieldsFromPslBody (Psl.Model.Body pslElements) =
      Field.pslFieldToEntityField <$> [field | (Psl.Model.ElementField field) <- pslElements]

getFields :: Entity -> [Field]
getFields = fields

getPslModelBody :: Entity -> Psl.Model.Body
getPslModelBody = pslModelBody

getIdField :: Entity -> Maybe Psl.Model.Field
getIdField = findIdField . getPslModelBody

isFieldUnique :: String -> Entity -> Maybe Bool
isFieldUnique fieldName = doesFieldHaveAttribute fieldName "unique"

doesFieldHaveAttribute :: String -> String -> Entity -> Maybe Bool
doesFieldHaveAttribute fieldName attrName entity =
  doesPslFieldHaveAttribute attrName <$> findPslFieldByName fieldName entity

findPslFieldByName :: String -> Entity -> Maybe Psl.Model.Field
findPslFieldByName fieldName Entity {pslModelBody = Psl.Model.Body elements} =
  find isField [field | (Psl.Model.ElementField field) <- elements]
  where
    isField Psl.Model.Field {_name = name} = name == fieldName

getIdBlockAttribute :: Entity -> Maybe Psl.Attribute.Attribute
getIdBlockAttribute = findIdBlockAttribute . getPslModelBody
