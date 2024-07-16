module Wasp.Analyzer.Prisma
  ( injectEntitiesFromPrismaSchema,
  )
where

import Wasp.Analyzer.Parser as Parser
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Generator.Model as Psl.Model.Generator

injectEntitiesFromPrismaSchema :: Psl.Schema.Schema -> Parser.AST -> Either a Parser.AST
injectEntitiesFromPrismaSchema schema ast = Right $ ast {Parser.astStmts = stmts ++ entityStmts}
  where
    entityStmts = makeEntityStmt <$> generatePrismaModelSources schema
    stmts = Parser.astStmts ast

type ModelName = String

type ModelBody = String

makeEntityStmt :: (ModelName, ModelBody) -> WithCtx Parser.Stmt
makeEntityStmt (name, body) = wrapWithCtx $ Parser.Decl "entity" name $ wrapWithCtx $ Parser.Quoter "psl" body
  where
    wrapWithCtx = WithCtx (Ctx mockSourceRegion)
    -- Since we didn't parse the entities from the Wasp source file
    -- but the Prisma schema file, we don't have a real source region.
    -- We mock the source region to be from 0 0 to 0 0.
    -- TODO: In the future, it would be nice to have the source region
    -- of the entity from the Prisma schema file.
    mockSourceRegion = SourceRegion (SourcePosition 0 0) (SourcePosition 0 0)

-- | Generates Prisma models source code so that it can be injected into Wasp AST.
generatePrismaModelSources :: Psl.Schema.Schema -> [(ModelName, ModelBody)]
generatePrismaModelSources schema =
  [ ( name,
      Psl.Model.Generator.generateModelBody body
    )
    | (Psl.Model.Model name body) <- Psl.Schema.getModels schema
  ]
