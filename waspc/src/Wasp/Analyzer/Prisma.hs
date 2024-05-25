module Wasp.Analyzer.Prisma where

import Wasp.Analyzer.Parser as Parser
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import qualified Wasp.Psl.Generator.Schema as Psl.Generator

injectEntitiesFromPrismaSchema :: Psl.Ast.Schema -> Parser.AST -> Either a Parser.AST
injectEntitiesFromPrismaSchema schema ast = Right $ ast {Parser.astStmts = stmts ++ entityStmts}
  where
    entityStmts = makeEntityStmt <$> generatePrismaModels schema
    stmts = Parser.astStmts ast

makeEntityStmt :: (String, String) -> WithCtx Parser.Stmt
makeEntityStmt (name, body) = wrapWithCtx $ Parser.Decl "entity" name $ wrapWithCtx $ Parser.Quoter "psl" body
  where
    wrapWithCtx = WithCtx (Ctx mockSourceRegion)
    -- Since we didn't parse the entities from the Wasp source file
    -- we don't have a real source region.
    -- TODO: In the future, it would be nice to have the source region
    -- of the entity from the Prisma schema file.
    mockSourceRegion = SourceRegion (SourcePosition 0 0) (SourcePosition 0 0)

-- | Generates Prisma models source code so that it can be injected into Wasp AST.
generatePrismaModels :: Psl.Ast.Schema -> [(String, String)]
generatePrismaModels schema =
  [ ( name,
      Psl.Generator.generateModelBody body
    )
    | (Psl.Ast.Model name body) <- Psl.Ast.getModels schema
  ]
