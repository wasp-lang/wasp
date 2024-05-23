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
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import Wasp.Psl.Util (doesPslFieldHaveAttribute, findIdBlockAttribute, findIdField)

data Entity = Entity
  { fields :: ![Field],
    pslModelBody :: !Psl.Ast.Body
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

makeEntity :: Psl.Ast.Body -> Entity
makeEntity body =
  Entity
    { fields = makeEntityFieldsFromPslModelBody body,
      pslModelBody = body
    }
  where
    makeEntityFieldsFromPslModelBody :: Psl.Ast.Body -> [Field]
    makeEntityFieldsFromPslModelBody (Psl.Ast.Body pslElements) =
      Field.pslModelFieldToEntityField <$> [field | (Psl.Ast.ElementField field) <- pslElements]

getFields :: Entity -> [Field]
getFields = fields

getPslModelBody :: Entity -> Psl.Ast.Body
getPslModelBody = pslModelBody

getIdField :: Entity -> Maybe Psl.Ast.Field
getIdField = findIdField . getPslModelBody

isFieldUnique :: String -> Entity -> Maybe Bool
isFieldUnique fieldName = doesFieldHaveAttribute fieldName "unique"

doesFieldHaveAttribute :: String -> String -> Entity -> Maybe Bool
doesFieldHaveAttribute fieldName attrName entity =
  doesPslFieldHaveAttribute attrName <$> findPslFieldByName fieldName entity

findPslFieldByName :: String -> Entity -> Maybe Psl.Ast.Field
findPslFieldByName fieldName Entity {pslModelBody = Psl.Ast.Body elements} =
  find isField [field | (Psl.Ast.ElementField field) <- elements]
  where
    isField Psl.Ast.Field {_name = name} = name == fieldName

getIdBlockAttribute :: Entity -> Maybe Psl.Ast.Attribute
getIdBlockAttribute = findIdBlockAttribute . getPslModelBody
