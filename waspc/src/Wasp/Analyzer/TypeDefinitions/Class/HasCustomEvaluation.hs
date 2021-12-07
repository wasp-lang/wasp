{-# LANGUAGE AllowAmbiguousTypes #-}

module Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation
  ( HasCustomEvaluation (..),
  )
where

import Data.Typeable (Typeable)
import Wasp.Analyzer.Evaluator.Evaluation.TypedExpr (TypedExprEvaluation)
import Wasp.Analyzer.Type (Type)

-- | For a Haskell type @a@, provides its Wasp @Type@ (via @waspType@ function)
-- | and its @Evaluation@ (via @evaluation@ function) that evaluates from
-- | @waspType@ into @a@.
-- | This class can be useful when using @makeDeclType@, since @makeDeclType@ checks
-- | for types with this class and if they have it, it uses their custom evaluation
-- | instead of trying to figure out their evaluation on its own.
class (Typeable a) => HasCustomEvaluation a where
  waspType :: Type
  evaluation :: TypedExprEvaluation a
