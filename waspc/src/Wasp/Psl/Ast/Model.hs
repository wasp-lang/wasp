{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Model
  ( Model (..),
    Body (..),
    Element (..),
    Field (..),
    FieldType (..),
    FieldTypeModifier (..),
    getFields,
  )
where

import Data.Data (Data)
import Wasp.Psl.Ast.Attribute (Attribute)
import Prelude hiding (Enum)

type Name = String

data Model
  = Model
      Name
      Body
  deriving (Show, Eq)

newtype Body = Body [Element]
  deriving (Show, Eq, Data)

data Element
  = ElementField Field
  | ElementBlockAttribute Attribute
  deriving (Show, Eq, Data)

-- TODO: To support attributes before the field,
--   we could just have `attrsBefore :: [[Attr]]`,
--   which represents lines, each one with list of attributes.
data Field = Field
  { _name :: String,
    _type :: FieldType,
    _typeModifiers :: [FieldTypeModifier],
    _attrs :: [Attribute]
  }
  deriving (Show, Eq, Data)

data FieldType
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

data FieldTypeModifier
  = List
  | Optional
  | UnsupportedOptionalList
  deriving (Show, Eq, Data)

getFields :: Model -> [Field]
getFields (Model _ (Body elements)) = [field | ElementField field <- elements]
