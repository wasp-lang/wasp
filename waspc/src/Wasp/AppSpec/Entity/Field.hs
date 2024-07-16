{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity.Field
  ( Field (..),
    FieldType (..),
    Composite (..),
    Scalar (..),
    pslFieldToEntityField,
  )
where

import Data.Data (Data)
import qualified Wasp.Psl.Ast.Model as Psl.Model

data Field = Field
  { fieldName :: !String,
    fieldType :: !FieldType
  }
  deriving (Show, Eq, Data)

data FieldType = FieldTypeScalar Scalar | FieldTypeComposite Composite
  deriving (Show, Eq, Data)

data Composite = Optional Scalar | List Scalar
  deriving (Show, Eq, Data)

data Scalar
  = String
  | Boolean
  | Int
  | BigInt
  | Float
  | Decimal
  | DateTime
  | Json
  | Bytes
  | -- | Name of the user-defined type.
    -- This could be another entity, or maybe an enum,
    -- we don't know here yet.
    UserType String
  | Unsupported String
  deriving (Show, Eq, Data)

pslFieldToEntityField :: Psl.Model.Field -> Field
pslFieldToEntityField pslField =
  Field
    { fieldName = Psl.Model._name pslField,
      fieldType =
        pslFieldTypeToEntityFieldType
          (Psl.Model._type pslField)
          (Psl.Model._typeModifiers pslField)
    }
  where
    pslFieldTypeToEntityFieldType :: Psl.Model.FieldType -> [Psl.Model.FieldTypeModifier] -> FieldType
    pslFieldTypeToEntityFieldType fType fTypeModifiers =
      let scalar = pslFieldTypeToScalar fType
       in case fTypeModifiers of
            [] -> FieldTypeScalar scalar
            [Psl.Model.List] -> FieldTypeComposite $ List scalar
            [Psl.Model.Optional] -> FieldTypeComposite $ Optional scalar
            _ -> error "Not a valid list of modifiers."

    pslFieldTypeToScalar :: Psl.Model.FieldType -> Scalar
    pslFieldTypeToScalar fType = case fType of
      Psl.Model.String -> String
      Psl.Model.Boolean -> Boolean
      Psl.Model.Int -> Int
      Psl.Model.BigInt -> BigInt
      Psl.Model.Float -> Float
      Psl.Model.Decimal -> Decimal
      Psl.Model.DateTime -> DateTime
      Psl.Model.Json -> Json
      Psl.Model.Bytes -> Bytes
      Psl.Model.UserType typeName -> UserType typeName
      Psl.Model.Unsupported typeName -> Unsupported typeName
