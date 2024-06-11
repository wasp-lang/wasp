{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Enum
  ( Enum (..),
    EnumElement (..),
  )
where

import Data.Data (Data)
import Wasp.Psl.Ast.Attribute (Attribute)
import Prelude hiding (Enum)

data Enum
  = Enum
      String
      [EnumElement]
  deriving (Show, Eq)

data EnumElement
  = EnumElementValue String [Attribute]
  | EnumElementBlockAttribute Attribute
  deriving (Show, Eq, Data)
