module Wasp.Analyzer
  ( -- * Overview

    -- |
    -- "Analyzer" turns a Prisma schema into "Wasp" AST entity declarations.
    --
    -- Historically it also parsed Wasp DSL source (@.wasp@ files) into "Wasp" AST,
    -- but that parser has been removed in favour of analyzing TypeScript config
    -- files (see "Wasp.Project.WaspFile.TypeScript"). What remains is the type
    -- system machinery ("Analyzer.TypeChecker", "Analyzer.Evaluator" and
    -- "Analyzer.TypeDefinitions") which is reused to derive entity declarations
    -- from the Prisma schema:
    --
    --  1. "Analyzer.Prisma" turns the Prisma schema into entity statements in
    --     "Analyzer.AST".
    --  2. "Analyzer.TypeChecker" enriches the AST with type information.
    --  3. "Analyzer.Evaluator" transforms the type-checked AST into "Wasp" AST.

    -- * API
    getEntityDecls,
    takeDecls,
    AnalyzeError (..),
    getErrorMessageAndCtx,
    SourcePosition (..),
  )
where

import Control.Arrow (left)
import Wasp.Analyzer.AST (AST (..))
import Wasp.Analyzer.AnalyzeError
  ( AnalyzeError (..),
    SourcePosition (..),
    getErrorMessageAndCtx,
  )
import Wasp.Analyzer.Evaluator (Decl, evaluate, takeDecls)
import Wasp.Analyzer.Prisma (parseEntityStatements)
import Wasp.Analyzer.StdTypeDefinitions (stdTypes)
import Wasp.Analyzer.TypeChecker (typeCheck)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema

getEntityDecls :: Psl.Schema.Schema -> Either [AnalyzeError] [Decl]
getEntityDecls schema =
  -- Since Wasp's AST includes entity declarations, the easiest way to get a list
  -- of all entities defined in the Prisma Schema is by:
  --   1. Creating an AST with (and only with) the declarations for the Prisma
  --   schema Entities.
  --   2. Type-checking that AST and returning the result.
  wrapAnalyzerError TypeError (typeCheck stdTypes astWithEntitiesOnly)
    >>= (wrapAnalyzerError EvaluationError . evaluate stdTypes)
  where
    astWithEntitiesOnly = AST $ parseEntityStatements schema

wrapAnalyzerError :: (e -> AnalyzeError) -> Either e a -> Either [AnalyzeError] a
wrapAnalyzerError makeError = left ((: []) . makeError)
