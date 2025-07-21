{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Enum
  ( Enum (..),
    Element (..),
  )
where

import Data.Data (Data)
import Wasp.Psl.Ast.Attribute (Attribute)
import Wasp.Psl.Ast.Common (Name)
import Wasp.Psl.Ast.OutputNode (OutputNode)
import Prelude hiding (Enum)

data Enum = Enum Name [OutputNode Element]
  deriving (Show, Eq)

data Element
  = ElementValue String [Attribute]
  | ElementBlockAttribute Attribute
  deriving (Show, Eq, Data)
