{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.Enum
  ( Enum (..),
    EnumElement (..),
  )
where

import Data.Data (Data)
import Wasp.Psl.Ast.Attribute (Attribute)
import Prelude hiding (Enum)

type EnumName = String

data Enum
  = Enum
      EnumName
      [EnumElement]
  deriving (Show, Eq)

data EnumElement
  = EnumElementValue String [Attribute]
  | EnumElementBlockAttribute Attribute
  deriving (Show, Eq, Data)
