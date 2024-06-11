{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Model
  ( Model (..),
    ModelBody (..),
    ModelElement (..),
    ModelField (..),
    ModelFieldType (..),
    ModelFieldTypeModifier (..),
    getModelFields,
  )
where

import Data.Data (Data)
import Wasp.Psl.Ast.Attribute (Attribute)
import Prelude hiding (Enum)

data Model
  = Model
      String
      ModelBody
  deriving (Show, Eq)

newtype ModelBody = ModelBody [ModelElement]
  deriving (Show, Eq, Data)

data ModelElement
  = ModelElementField ModelField
  | ModelElementBlockAttribute Attribute
  deriving (Show, Eq, Data)

-- TODO: To support attributes before the field,
--   we could just have `attrsBefore :: [[Attr]]`,
--   which represents lines, each one with list of attributes.
data ModelField = ModelField
  { _name :: String,
    _type :: ModelFieldType,
    _typeModifiers :: [ModelFieldTypeModifier],
    _attrs :: [Attribute]
  }
  deriving (Show, Eq, Data)

data ModelFieldType
  = String
  | Boolean
  | Int
  | BigInt
  | Float
  | Decimal
  | DateTime
  | Json
  | Bytes
  | Unsupported String
  | UserType String
  deriving (Show, Eq, Data)

data ModelFieldTypeModifier
  = List
  | Optional
  | UnsupportedOptionalList
  deriving (Show, Eq, Data)

getModelFields :: Model -> [ModelField]
getModelFields (Model _ (ModelBody elements)) = [field | ModelElementField field <- elements]
