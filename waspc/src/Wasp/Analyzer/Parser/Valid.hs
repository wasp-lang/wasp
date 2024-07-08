module Wasp.Analyzer.Parser.Valid
  ( validateAst,
  )
where

import Data.List (find)
import qualified Wasp.Analyzer.Parser as P
import Wasp.Analyzer.StdTypeDefinitions.Entity (entityDeclTypeName)

validateAst :: P.AST -> Either (String, P.Ctx) P.AST
validateAst = validateNoEntityDeclInWaspFile

validateNoEntityDeclInWaspFile :: P.AST -> Either (String, P.Ctx) P.AST
validateNoEntityDeclInWaspFile ast@(P.AST stmts) = case findEntityStmt stmts of
  Just (P.WithCtx ctx _) -> Left (entitiesNoLongerSupportedError, ctx)
  Nothing -> Right ast
  where
    findEntityStmt :: [P.WithCtx P.Stmt] -> Maybe (P.WithCtx P.Stmt)
    findEntityStmt =
      find
        ( \(P.WithCtx _ (P.Decl declTypeName _ _)) -> declTypeName == entityDeclTypeName
        )

    entitiesNoLongerSupportedError :: String
    entitiesNoLongerSupportedError =
      "Entities can no longer be defined in the .wasp file. You should migrate your entities to the schema.prisma file. Read more: https://wasp-lang.dev/docs/migrate-from-0-13-to-0-14#migrate-to-the-new-schemaprisma-file"
