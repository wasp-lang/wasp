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

data EnumType = EnumType
  { etName :: String,
    etVariants :: [String]
  }

data DeclType = DeclType
  { dtName :: String,
    dtBodyType :: Type,
    -- | Evaluates a Wasp "TypedExpr" to a Wasp AST declaration, or an error.
    --
    -- For @dtDeclFromAST typeDefs bindings name expr@:
    -- - "typeDefs" is the type definitions used in the Analyzer
    -- - "bindings" contains the values of all the declarations evaluated so far
    -- - "name" is name of the declaration
    -- - "expr" is the expression that should be evaluated by this function
    --
    -- __Examples__
    --
    -- Wasp code @test Example 4@ would call this function with
    -- @dtDeclFromAST declType typeDefs bindings "Example" (NumberLiteral 4)@
    dtDeclFromAST :: TypeDefinitions -> M.HashMap String Decl -> String -> TypedExpr -> Either EvaluationError Decl
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
  { declTypes :: M.HashMap String DeclType,
    enumTypes :: M.HashMap String EnumType
    -- TODO: In the future, add quoters to the type definitions
  }
