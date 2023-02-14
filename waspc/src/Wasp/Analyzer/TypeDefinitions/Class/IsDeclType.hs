{-# LANGUAGE AllowAmbiguousTypes #-}

module Wasp.Analyzer.TypeDefinitions.Class.IsDeclType
  ( IsDeclType (..),
  )
where

import Data.Typeable (Typeable)
import Wasp.Analyzer.Evaluator.Bindings (Bindings)
import Wasp.Analyzer.Evaluator.EvaluationError (EvaluationError)
import Wasp.Analyzer.TypeChecker.AST (TypedExpr, WithCtx)
import Wasp.Analyzer.TypeDefinitions.Internal (DeclType, TypeDefinitions)
import qualified Wasp.AppSpec.Core.Decl as AppSpecDecl

-- | Marks Haskell type as a representation of a specific Wasp declaration type.
-- This is supposed to be used on types from @AppSpec@ (the main Wasp IR)
-- in order to enrich them with information on how are they to be analyzed
-- (and therefore make them part of the Wasp language).
--
-- To put it very practically, if you make a Haskell type (usually from @AppSpec@)
-- an instance of this class, and if you add it to "Wasp.Analyzer.StdTypeDefinitions",
-- you are telling Analyzer that there is a new declaration type in Wasp language with
-- name, Wasp type and evaluation from that Wasp type into Haskell type as specified
-- by this instance, and therefore you just added a new declaration type to the
-- Wasp language and Analyzer will include it in the final result it produces.
--
-- NOTE: If this Haskell type satisfies certain requirements, the IsDeclType instance for it
-- can be automatically derived from its shape by using
-- 'Wasp.Analyzer.TypeDefinitions.TH.makeDeclType'.
class (Typeable a, AppSpecDecl.IsDecl a) => IsDeclType a where
  declType :: DeclType

  -- | Evaluates a given Wasp "TypedExpr" to @a@, assuming given typed
  -- expression is a subtype of declaration type described by @dtBodyType .
  -- declType@ and @dtName . declType@ (otherwise throws an error).
  --
  -- For @declEvaluate typeDefs bindings declBodyExpr@:
  -- - "typeDefs" is the type definitions used in the Analyzer
  -- - "bindings" contains the values of all the declarations evaluated so far
  -- - "declBodyExpr" is the expression describing declaration body,
  --   that should be evaluated by this function
  --
  -- __Examples__
  --
  -- Imagine that we have Wasp code @test Example 4@, and we have @instance IsDeclType Test@.
  -- Here, @test@ is declaration type name, @Example@ is declaration name,
  -- and @4@ is declaration body.
  -- @declEvaluate@ function would then be called somewhat like:
  -- @declEvaluate @Test typeDefs bindings (NumberLiteral 4)@
  declEvaluate :: TypeDefinitions -> Bindings -> WithCtx TypedExpr -> Either EvaluationError a
