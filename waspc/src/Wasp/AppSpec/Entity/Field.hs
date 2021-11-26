{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Entity.Field
  ( Field (..),
    FieldType (..),
    Composite (..),
    Scalar (..),
    pslModelFieldToEntityField,
  )
where

import Data.Data (Data)
import qualified Wasp.Psl.Ast.Model as PslModel

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

pslModelFieldToEntityField :: PslModel.Field -> Field
pslModelFieldToEntityField pslField =
  Field
    { fieldName = PslModel._name pslField,
      fieldType =
        pslFieldTypeToEntityFieldType
          (PslModel._type pslField)
          (PslModel._typeModifiers pslField)
    }
  where
    pslFieldTypeToEntityFieldType :: PslModel.FieldType -> [PslModel.FieldTypeModifier] -> FieldType
    pslFieldTypeToEntityFieldType fType fTypeModifiers =
      let scalar = pslFieldTypeToScalar fType
       in case fTypeModifiers of
            [] -> FieldTypeScalar scalar
            [PslModel.List] -> FieldTypeComposite $ List scalar
            [PslModel.Optional] -> FieldTypeComposite $ Optional scalar
            _ -> error "Not a valid list of modifiers."

    pslFieldTypeToScalar :: PslModel.FieldType -> Scalar
    pslFieldTypeToScalar fType = case fType of
      PslModel.String -> String
      PslModel.Boolean -> Boolean
      PslModel.Int -> Int
      PslModel.BigInt -> BigInt
      PslModel.Float -> Float
      PslModel.Decimal -> Decimal
      PslModel.DateTime -> DateTime
      PslModel.Json -> Json
      PslModel.Bytes -> Bytes
      PslModel.UserType typeName -> UserType typeName
      PslModel.Unsupported typeName -> Unsupported typeName
