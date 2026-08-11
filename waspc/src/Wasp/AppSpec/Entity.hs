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

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.=))
import Data.Data (Data)
import Data.List (intercalate)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Generator.Model (generateModelFieldTypeAndModifiers)
import Wasp.Psl.Util (findIdBlockAttribute, findIdField, getModelFields)

newtype Entity = Entity
  { pslModelBody :: Psl.Model.Body
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

instance FromJSON Entity where
  parseJSON = const $ fail "Entity declarations in wasp are deprecated, entities are now defined via prisma.schema file."

instance ToJSON Entity where
  toJSON entity =
    object
      [ "fields" .= map fieldToJSON (getFields entity)
      ]
    where
      fieldToJSON field =
        object
          [ "name" .= Psl.Model._name field,
            "type" .= generateModelFieldTypeAndModifiers field
          ]

instance Inspectable Entity where
  inspect entity =
    [ InspectionEntry
        "Entities"
        [("Fields", intercalate ", " $ Psl.Model._name <$> getFields entity)]
    ]

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
