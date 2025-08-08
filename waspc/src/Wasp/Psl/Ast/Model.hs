{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Model
  ( Model (..),
    Body (..),
    Element (..),
    Field (..),
    FieldType (..),
    FieldTypeModifier (..),
    getName,
  )
where

import Data.Data (Data)
import Wasp.Psl.Ast.Attribute (Attribute)
import Wasp.Psl.Ast.Common (Name)
import Wasp.Psl.Ast.WithCtx (WithCtx)
import Prelude hiding (Enum)

data Model
  = Model
      Name
      Body
  deriving (Show, Eq)

newtype Body = Body [WithCtx Element]
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
  deriving (Show, Eq, Data)

getName :: Model -> Name
getName (Model name _) = name
