{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity
  ( makeEntity,
    Entity,
    getFields,
    getPslModelBody,
    getIdField,
    getIdBlockAttribute,
    showEntityRefs,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.=))
import Data.Data (Data)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (..))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Entity.Field (Field)
import qualified Wasp.AppSpec.Entity.Field as Field
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Psl.Generator.Model (generateModelBody)
import Wasp.Psl.Util (findIdBlockAttribute, findIdField)

data Entity = Entity
  { fields :: ![Field],
    pslModelBody :: !Psl.Model.Body
  }
  deriving (Show, Eq, Data)

instance IsDecl Entity

instance FromJSON Entity where
  parseJSON = const $ fail "Entity declarations in wasp are deprecated, entities are now defined via prisma.schema file."

-- | Entities are not part of the JSON wire format coming from the TS spec
-- (they are built from the Prisma schema), so this shape exists only for
-- output (e.g. `wasp inspect --json`) and is deliberately not parseable back.
instance ToJSON Entity where
  toJSON entity =
    object
      [ "fields" .= fields entity,
        "pslSource" .= generateModelBody (pslModelBody entity)
      ]

instance Inspectable Entity where
  inspectionSection = "Entities"
  inspect (name, entity) =
    InspectionEntry [name, intercalate ", " $ showField <$> fields entity]
    where
      showField field = Field.fieldName field ++ " " ++ Field.showFieldType (Field.fieldType field)

-- | Renders a cell like "uses Task, Tag" for declarations that operate on entities.
showEntityRefs :: Maybe [Ref Entity] -> String
showEntityRefs entityRefs = case refName <$> fromMaybe [] entityRefs of
  [] -> ""
  names -> "uses " ++ intercalate ", " names

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
