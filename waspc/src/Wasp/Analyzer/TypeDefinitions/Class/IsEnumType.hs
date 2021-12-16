{-# LANGUAGE AllowAmbiguousTypes #-}

module Wasp.Analyzer.TypeDefinitions.Class.IsEnumType
  ( IsEnumType (..),
  )
where

import Data.Typeable (Typeable)
import Wasp.Analyzer.TypeDefinitions.Internal (EnumType)

-- | Marks Haskell type as a representation of a specific Wasp enum type.
-- Analogous to IsDeclType, but for enums.
--
-- NOTE: If this Haskell type satisfies certain requirements, the IsEnumType instance for it
-- can be automatically derived from its shape by using 'Analyzer.Evaluator.TH.makeEnumType'.
class Typeable a => IsEnumType a where
  enumType :: EnumType

  -- | Converts a string to a Haskell value of this type.
  --
  -- @mapM_ enumEvaluate (etVariants enumType) == Just ()@ should be true
  -- for all instances of "IsEnumType".
  --
  -- __Examples__
  --
  -- >>> data Example = Foo | Bar
  -- >>> instance IsEnumType Example where {- omitted -}
  -- >>> enumEvaluate "Foo"
  -- Just Foo
  enumEvaluate :: String -> Maybe a
