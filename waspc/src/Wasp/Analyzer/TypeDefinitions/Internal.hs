module Wasp.Analyzer.TypeDefinitions.Internal
  ( EnumType (..),
    DeclType (..),
    TypeDefinitions (..),
  )
where

import qualified Data.HashMap.Strict as M
import Wasp.Analyzer.Evaluator.Bindings (Bindings, DeclName)
import Wasp.Analyzer.Evaluator.EvaluationError (EvaluationError)
import Wasp.Analyzer.Type (Type)
import Wasp.Analyzer.TypeChecker.AST (TypedExpr)
import Wasp.AppSpec.Core.Decl (Decl)

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
    -- Check @declEvaluate@ of @IsDeclType@ typeclass for more information,
    -- since @dtEvaluate@ is really a value-level version of @declEvaluate@.
    dtEvaluate :: TypeDefinitions -> Bindings -> DeclName -> TypedExpr -> Either EvaluationError Decl
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

type DeclTypeName = String

type EnumTypeName = String
