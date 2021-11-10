module Wasp.Wasp.Entity
  ( Entity (..),
    Field (..),
    FieldType (..),
    Scalar (..),
    Composite (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Wasp.Psl.Ast.Model

data Entity = Entity
  { _name :: !String,
    _fields :: ![Field],
    _pslModelBody :: !Wasp.Psl.Ast.Model.Body
  }
  deriving (Show, Eq)

data Field = Field
  { _fieldName :: !String,
    _fieldType :: !FieldType
  }
  deriving (Show, Eq)

data FieldType = FieldTypeScalar Scalar | FieldTypeComposite Composite
  deriving (Show, Eq)

data Composite = Optional Scalar | List Scalar
  deriving (Show, Eq)

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
  deriving (Show, Eq)

instance ToJSON Entity where
  toJSON entity =
    object
      [ "name" .= _name entity,
        "fields" .= show (_fields entity),
        "pslModelBody" .= show (_pslModelBody entity)
      ]
