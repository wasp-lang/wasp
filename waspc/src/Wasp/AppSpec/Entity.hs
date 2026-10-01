{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity
  ( makeEntityDecls,
    makeEntity,
    Entity,
    getName,
    getPslModel,
    getFields,
    getPslModelBody,
    getIdField,
    getIdBlockAttribute,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.=))
import Data.Data (Data)
import Data.List (intercalate)
import Wasp.AppSpec.Core.Decl (Decl)
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Core.IsDecl (IsDecl (..))
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Psl.Generator.Model (generateModelFieldTypeAndModifiers)
import Wasp.Psl.Util (findIdBlockAttribute, findIdField, getModelFields)

newtype Entity = Entity
  { pslModel :: Psl.Model.Model
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity where
  declName = getName

instance FromJSON Entity where
  parseJSON = const $ fail "Entity declarations in wasp are deprecated, entities are now defined via prisma.schema file."

instance ToJSON Entity where
  toJSON entity =
    object
      [ "name" .= getName entity,
        "fields" .= map fieldToJSON (getFields entity)
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
        [ ("Name", getName entity),
          ("Fields", intercalate ", " $ Psl.Model._name <$> getFields entity)
        ]
    ]

-- | Constructs entity declarations from parsed Prisma models.
makeEntityDecls :: Psl.Schema.Schema -> [Decl]
makeEntityDecls = map (Decl.makeDecl . makeEntity . Psl.WithCtx.getNode) . Psl.Schema.getModels

makeEntity :: Psl.Model.Model -> Entity
makeEntity = Entity

getName :: Entity -> String
getName = Psl.Model.getName . pslModel

getPslModel :: Entity -> Psl.Model.Model
getPslModel = pslModel

getFields :: Entity -> [Psl.Model.Field]
getFields = getModelFields . getPslModelBody

getPslModelBody :: Entity -> Psl.Model.Body
getPslModelBody = Psl.Model.getBody . pslModel

getIdField :: Entity -> Maybe Psl.Model.Field
getIdField = findIdField . getPslModelBody

getIdBlockAttribute :: Entity -> Maybe Psl.Attribute.Attribute
getIdBlockAttribute = findIdBlockAttribute . getPslModelBody
