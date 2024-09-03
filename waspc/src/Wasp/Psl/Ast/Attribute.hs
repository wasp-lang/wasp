{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Attribute
  ( Attribute (..),
    isNativeDbTypeAttr,
  )
where

import Data.Data (Data)
import Data.List (isPrefixOf)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import Prelude hiding (Enum)

-- NOTE: We don't differentiate "native database type" attributes from normal attributes right now,
--   they are all represented with `data Attribute`.
--   We just represent them as a normal attribute with attrName being e.g. "db.VarChar".
-- TODO: In the future, we might want to be "smarter" about this and actually have a special representation
--   for them -> but let's see if that will be needed.
data Attribute = Attribute
  { _attrName :: String,
    _attrArgs :: [Psl.Argument.Argument]
  }
  deriving (Show, Eq, Data)

-- | @db.Uuid or @db.String or @db.VarChar are examples of native db types.
isNativeDbTypeAttr :: Attribute -> Bool
isNativeDbTypeAttr = ("db." `isPrefixOf`) . _attrName
