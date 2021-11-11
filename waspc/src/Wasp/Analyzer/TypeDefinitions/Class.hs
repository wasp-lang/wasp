{-# LANGUAGE AllowAmbiguousTypes #-}

module Wasp.Analyzer.TypeDefinitions.Class
  ( IsDeclType (..),
    IsEnumType (..),
  )
where

import Data.Typeable (Typeable)
import Wasp.Analyzer.Evaluator.Bindings (Bindings)
import Wasp.Analyzer.Evaluator.EvaluationError (EvaluationError)
import Wasp.Analyzer.TypeChecker.AST (TypedExpr)
import Wasp.Analyzer.TypeDefinitions.Internal (DeclType, EnumType, TypeDefinitions)
import qualified Wasp.AppSpec.Core.Decl as AppSpecDecl

-- | Marks Haskell type as a representation of a specific Wasp declaration type.
-- This is supposed to be used on types from Wasp AST (the IR between Analyzer and Generator)
-- in order to enrich them with information on how are they to be analyzed
-- (and therefore make them part of the Wasp language).
--
-- NOTE: If this Haskell type satisfies certain requirements, the IsDeclType instance for it
-- can be automatically derived from its shape by using 'Analyzer.Evaluator.TH.makeDeclType'.
class (Typeable a, AppSpecDecl.IsDecl a) => IsDeclType a where
  declType :: DeclType

  -- | Evaluates a given Wasp "TypedExpr" to a value of this type, assuming it is of
  -- declaration type described by (dtBodyType . declType) and (dtName . declType) (otherwise throws an error).
  --
  -- For @declEvaluate typeDefs bindings declBodyExpr@:
  -- - "typeDefs" is the type definitions used in the Analyzer
  -- - "bindings" contains the values of all the declarations evaluated so far
  -- - "declBodyExpr" is the expression describing declaration body, that should be evaluated by this function
  --
  -- __Examples__
  --
  -- Imagine that we have Wasp code @test Example 4@, and we have @instance IsDeclType Test@.
  -- Here, @test@ is declaration type name, @Example@ is declaration name, and @4@ is declaration body.
  -- @declEvaluate@ function would then be called somewhat like:
  -- @declEvaluate @Test typeDefs bindings (NumberLiteral 4)@
  declEvaluate :: TypeDefinitions -> Bindings -> TypedExpr -> Either EvaluationError a

-- | Marks Haskell type as a representation of a specific Wasp enum type.
-- Analogous to IsDeclType, but for enums.
--
-- NOTE: If this Haskell type satisfies certain requirements, the IsEnumType instance for it
-- can be automatically derived from its shape by using 'Analyzer.Evaluator.TH.makeEnumType'.
class Typeable a => IsEnumType a where
  enumType :: EnumType

  -- | Converts a string to a Haskell value of this type.
  --
  -- @mapM_ enumEvaluate (etVariants enumType) == Right ()@ should be true
  -- for all instances of "IsEnumType".
  --
  -- __Examples__
  --
  -- >>> data Example = Foo | Bar
  -- >>> instance IsEnumType Example where {- omitted -}
  -- >>> enumEvaluate "Foo"
  -- Foo
  enumEvaluate :: String -> Either EvaluationError a
