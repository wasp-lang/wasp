{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Attribute
  ( Attribute (..),
    AttributeArg (..),
    AttrArgValue (..),
  )
where

import Data.Data (Data)
import Prelude hiding (Enum)

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

data AttributeArg
  = AttrArgNamed String AttrArgValue
  | AttrArgUnnamed AttrArgValue
  deriving (Show, Eq, Data)

data AttrArgValue
  = AttrArgString String
  | AttrArgIdentifier String
  | AttrArgFunc String
  | AttrArgFieldRefList [String]
  | AttrArgNumber String
  | AttrArgUnknown String
  deriving (Show, Eq, Data)
