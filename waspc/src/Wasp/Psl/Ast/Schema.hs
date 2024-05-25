{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Schema where

import Data.Data (Data)

data Schema = Schema [SchemaElement]
  deriving (Show, Eq)

data SchemaElement = SchemaModel Model | SchemaEnum PrismaEnum | SchemaDatasource Datasource | SchemaGenerator Generator
  deriving (Show, Eq)

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

data PrismaEnum
  = PrismaEnum
      String
      -- ^ Name of the enum
      [EnumField]
  deriving (Show, Eq)

data EnumField = EnumValue String [Attribute] | EnumBlockAttribute Attribute
  deriving (Show, Eq, Data)

data Datasource
  = Datasource
      String
      -- ^ Name of the datasource
      [ConfigBlockKeyValue]
      -- ^ Content of the datasource
  deriving (Show, Eq)

data Generator
  = Generator
      String
      -- ^ Name of the generator
      [ConfigBlockKeyValue]
      -- ^ Content of the generator
  deriving (Show, Eq)

-- | Represents a key-value pair in a config block.
--  For example, in the following config block:
--  ```
--  generator client {
--    provider = "prisma-client-js"
--  }
--  ```
--  The key-value pair would be `ConfigBlockKeyValue "provider" "prisma-client-js"`.
data ConfigBlockKeyValue = ConfigBlockKeyValue String String
  deriving (Show, Eq)

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

getModels :: Schema -> [Model]
getModels (Schema elements) = [model | SchemaModel model <- elements]

getEnums :: Schema -> [PrismaEnum]
getEnums (Schema elements) = [enum | SchemaEnum enum <- elements]

getDatasources :: Schema -> [Datasource]
getDatasources (Schema elements) = [datasource | SchemaDatasource datasource <- elements]

getGenerators :: Schema -> [Generator]
getGenerators (Schema elements) = [generator | SchemaGenerator generator <- elements]
