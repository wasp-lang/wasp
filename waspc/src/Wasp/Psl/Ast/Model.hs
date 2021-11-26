{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Model where

import Data.Data (Data)

data Model
  = Model
      String
      -- ^ Name of the model
      Body
  deriving (Show, Eq)

newtype Body = Body [Element]
  deriving (Show, Eq, Data)

data Element = ElementField Field | ElementBlockAttribute Attribute
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

data FieldTypeModifier = List | Optional
  deriving (Show, Eq, Data)

-- NOTE: We don't differentiate "native database type" attributes from normal attributes right now,
--   they are all represented with `data Attribute`.
--   We just represent them as a normal attribute with attrName being e.g. "db.VarChar".
-- TODO: In the future, we might want to be "smarter" about this and actually have a special representation
--   for them -> but let's see if that will be needed.
data Attribute = Attribute
  { _attrName :: String,
    _attrArgs :: [AttributeArg]
  }
  deriving (Show, Eq, Data)

data AttributeArg = AttrArgNamed String AttrArgValue | AttrArgUnnamed AttrArgValue
  deriving (Show, Eq, Data)

data AttrArgValue
  = AttrArgString String
  | AttrArgIdentifier String
  | AttrArgFunc String
  | AttrArgFieldRefList [String]
  | AttrArgNumber String
  | AttrArgUnknown String
  deriving (Show, Eq, Data)
