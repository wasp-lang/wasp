{-# LANGUAGE AllowAmbiguousTypes #-}

module Analyzer.TypeDefinitions.Class
  ( IsDeclType (..),
    IsEnumType (..),
  )
where

import Analyzer.Evaluator.EvaluationError (EvaluationError)
import Analyzer.TypeDefinitions.Type
import Data.Typeable (Typeable)

-- | Marks Haskell type as a representation of a specific Wasp declaration type.
-- This is supposed to be used on types from Wasp AST (the IR between Analyzer and Generator)
-- in order to enrich them with information on how are they to be analyzed
-- (and therefore make them part of the Wasp language).
--
-- NOTE: If this Haskell type satisfies certain requirements, the IsDeclType instance for it
-- can be automatically derived from its shape by using 'Analyzer.Evaluator.TH.makeDeclType'.
class Typeable a => IsDeclType a where
  declType :: DeclType

-- TODO: Implement declEvaluate here? We don't really need it, but that way
--   it would be consistent with IsEnumType below, and if we need it we have it.
--   dtEvaluate would then use declEvaluate.

-- | Marks Haskell type as a representation of a specific Wasp enum type.
-- Analogous to IsDeclType, but for enums.
--
-- NOTE: If this Haskell type satisfies certain requirements, the IsEnumType instance for it
-- can be automatically derived from its shape by using 'Analyzer.Evaluator.TH.makeEnumType'.
class Typeable a => IsEnumType a where
  enumType :: EnumType

  -- TODO: Elevate this function so it takes TypedExpr instead of a String?
  --   By doing so it will be more similar to dtEvaluate from DeclType,
  --   and then we can also rename it to enumEvaluate.

  -- | Converts a string to a Haskell value of this type.
  --
  -- @mapM_ enumTypeFromVariant (etVariants enumType) == Right ()@ should be true
  -- for all instances of "IsEnumType".
  --
  -- __Examples__
  --
  -- >>> data Example = Foo | Bar
  -- >>> instance IsEnumType Example where {- omitted -}
  -- >>> enumTypeFromVariant "Foo"
  -- Foo
  enumTypeFromVariant :: String -> Either EvaluationError a
