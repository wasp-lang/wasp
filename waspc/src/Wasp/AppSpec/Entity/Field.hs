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
import qualified Wasp.Psl.Ast.Schema as Psl.Ast

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

pslModelFieldToEntityField :: Psl.Ast.Field -> Field
pslModelFieldToEntityField pslField =
  Field
    { fieldName = Psl.Ast._name pslField,
      fieldType =
        pslFieldTypeToEntityFieldType
          (Psl.Ast._type pslField)
          (Psl.Ast._typeModifiers pslField)
    }
  where
    pslFieldTypeToEntityFieldType :: Psl.Ast.FieldType -> [Psl.Ast.FieldTypeModifier] -> FieldType
    pslFieldTypeToEntityFieldType fType fTypeModifiers =
      let scalar = pslFieldTypeToScalar fType
       in case fTypeModifiers of
            [] -> FieldTypeScalar scalar
            [Psl.Ast.List] -> FieldTypeComposite $ List scalar
            [Psl.Ast.Optional] -> FieldTypeComposite $ Optional scalar
            _ -> error "Not a valid list of modifiers."

    pslFieldTypeToScalar :: Psl.Ast.FieldType -> Scalar
    pslFieldTypeToScalar fType = case fType of
      Psl.Ast.String -> String
      Psl.Ast.Boolean -> Boolean
      Psl.Ast.Int -> Int
      Psl.Ast.BigInt -> BigInt
      Psl.Ast.Float -> Float
      Psl.Ast.Decimal -> Decimal
      Psl.Ast.DateTime -> DateTime
      Psl.Ast.Json -> Json
      Psl.Ast.Bytes -> Bytes
      Psl.Ast.UserType typeName -> UserType typeName
      Psl.Ast.Unsupported typeName -> Unsupported typeName
