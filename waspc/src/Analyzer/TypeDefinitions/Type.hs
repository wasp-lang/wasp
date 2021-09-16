module Analyzer.TypeDefinitions.Type
  ( EnumType (..),
    DeclType (..),
    TypeDefinitions (..),
  )
where

import Analyzer.Evaluator.Decl (Decl)
import Analyzer.Evaluator.EvaluationError (EvaluationError)
import Analyzer.Type (Type)
import Analyzer.TypeChecker.AST (TypedExpr)
import qualified Data.HashMap.Strict as M

-- TODO: HashMap here is really bindings, and that concept already exists in Evalutor,
-- so should we somehow name them that way? Maybe even combine TypeDefinitions and bindings into
-- and eval context (EvalCtx) and pass that as a single thing? Yes, I think we should do this, especially
-- the EvalCtx part.

-- | Describes a specific declaration type in Wasp.
-- For example, such declaration type could be @page@, or @route@.
-- Declaration type is defined by its name (@dtName@) and the type of its body (@dtBodyType@).
-- Additionally, we also need to know how to evaluate it from the type checked AST into the Wasp AST,
-- which is what @dtEvaluate@ function does.
data DeclType = DeclType
  { dtName :: DeclTypeName,
    dtBodyType :: Type,
    -- | Evaluates a given Wasp "TypedExpr" to a Wasp AST declaration, assuming it is of
    -- declaration type described by dtBodyType and dtName (otherwise throws an error).
    --
    -- For @dtEvaluate typeDefs bindings declName declBodyExpr@:
    -- - "typeDefs" is the type definitions used in the Analyzer
    -- - "bindings" contains the values of all the declarations evaluated so far
    -- - "declName" is name of the declaration
    -- - "declBodyExpr" is the expression describing declaration body, that should be evaluated by this function
    --
    -- __Examples__
    --
    -- Imagine that we have Wasp code @test Example 4@.
    -- Here, @test@ is declaration type name, @Example@ is declaration name, and @4@ is declaration body.
    -- @dtEvaluate@ function would then be called somewhat like:
    -- @dtEvaluate declType typeDefs bindings "Example" (NumberLiteral 4)@
    -- where @declType@ is the one describing @test@ declaration type.
    dtEvaluate :: TypeDefinitions -> M.HashMap DeclName Decl -> DeclName -> TypedExpr -> Either EvaluationError Decl
  }

-- | Describes a specific enum type in Wasp.
-- For example, such enum type could be @AuthMethod@, or @DbType@.
-- Enum type is defined by its name (@etName@) and its variations (possible values) (@etVariants@).
-- Example: @EnumType { etName = "AuthMethod", etVariants = ["Google", "EmailAndPassword", "LinkedIn"] }@.
data EnumType = EnumType
  { etName :: EnumTypeName,
    etVariants :: [String]
  }

-- | The parser, type-checking, and evaluator require information about declaration,
--   enum, and quoter types, but the specific types are not hardcoded into each
--   phase of the analyzer.
--
--   Instead, this information is injected into them via 'TypeDefinitions',
--   which defines in one place the specific declaration/enum/quoter types.
--
--   This enables us to easily modify / add / remove specific types as Wasp evolves as a language without
--   having to touch the core functionality of the Analyzer.
data TypeDefinitions = TypeDefinitions
  { declTypes :: M.HashMap DeclTypeName DeclType,
    enumTypes :: M.HashMap EnumTypeName EnumType
    -- TODO: In the future, add quoters to the type definitions
  }

type DeclName = String

type DeclTypeName = String

type EnumTypeName = String
