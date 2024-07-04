module Wasp.Analyzer.Parser.Valid
  ( validateAst,
  )
where

import Data.List (find)
import qualified Wasp.Analyzer.Parser as P

validateAst :: P.AST -> Either (String, P.Ctx) P.AST
validateAst = validateNoEntityDeclInWaspFile

validateNoEntityDeclInWaspFile :: P.AST -> Either (String, P.Ctx) P.AST
validateNoEntityDeclInWaspFile ast@(P.AST stmts) = case findEntityDecl stmts of
  Just (P.WithCtx ctx _) -> Left (entitiesNoLongerSupportedError, ctx)
  Nothing -> Right ast
  where
    findEntityDecl :: [P.WithCtx P.Stmt] -> Maybe (P.WithCtx P.Stmt)
    findEntityDecl = find isEntityDecl

    isEntityDecl :: P.WithCtx P.Stmt -> Bool
    isEntityDecl = \case
      P.WithCtx _ (P.Decl "entity" _ _) -> True
      _ -> False

    entitiesNoLongerSupportedError :: String
    entitiesNoLongerSupportedError =
      "Entities can no longer be defined in the .wasp file. You should migrate your entities to the schema.prisma file. Read more: https://wasp-lang.dev/docs/migrate-from-0-13-to-0-14#migrate-to-the-new-schemaprisma-file"
