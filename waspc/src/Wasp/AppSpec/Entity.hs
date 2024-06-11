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
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Entity.Field (Field)
import qualified Wasp.AppSpec.Entity.Field as Field
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Util (doesPslFieldHaveAttribute, findIdBlockAttribute, findIdField)

data Entity = Entity
  { fields :: ![Field],
    pslModelBody :: !Psl.Model.ModelBody
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

makeEntity :: Psl.Model.ModelBody -> Entity
makeEntity body =
  Entity
    { fields = makeEntityFieldsFromPslModelBody body,
      pslModelBody = body
    }
  where
    makeEntityFieldsFromPslModelBody :: Psl.Model.ModelBody -> [Field]
    makeEntityFieldsFromPslModelBody (Psl.Model.ModelBody pslElements) =
      Field.pslModelFieldToEntityField <$> [field | (Psl.Model.ModelElementField field) <- pslElements]

getFields :: Entity -> [Field]
getFields = fields

getPslModelBody :: Entity -> Psl.Model.ModelBody
getPslModelBody = pslModelBody

getIdField :: Entity -> Maybe Psl.Model.ModelField
getIdField = findIdField . getPslModelBody

isFieldUnique :: String -> Entity -> Maybe Bool
isFieldUnique fieldName = doesFieldHaveAttribute fieldName "unique"

doesFieldHaveAttribute :: String -> String -> Entity -> Maybe Bool
doesFieldHaveAttribute fieldName attrName entity =
  doesPslFieldHaveAttribute attrName <$> findPslFieldByName fieldName entity

findPslFieldByName :: String -> Entity -> Maybe Psl.Model.ModelField
findPslFieldByName fieldName Entity {pslModelBody = Psl.Model.ModelBody elements} =
  find isField [field | (Psl.Model.ModelElementField field) <- elements]
  where
    isField Psl.Model.ModelField {_name = name} = name == fieldName

getIdBlockAttribute :: Entity -> Maybe Psl.Attribute.Attribute
getIdBlockAttribute = findIdBlockAttribute . getPslModelBody
