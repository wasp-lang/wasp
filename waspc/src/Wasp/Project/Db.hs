module Wasp.Project.Db
  ( makeDevDatabaseUrl,
    databaseUrlEnvVarName,
    getDbSystemFromPrismaSchema,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.App.Db as AS.Db
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Db.Dev.Postgres as DevPostgres
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
import Wasp.Psl.Util (findPrismaConfigBlockKeyValue)

makeDevDatabaseUrl ::
  Path' Abs (Dir WaspProjectDir) ->
  AS.Db.DbSystem ->
  [AS.Decl] ->
  Maybe String
makeDevDatabaseUrl waspProjectDir dbSystem decls = do
  (appName, _) <- AS.getApp decls
  case dbSystem of
    AS.App.Db.PostgreSQL -> Just $ DevPostgres.makeDevConnectionUrl waspProjectDir appName
    _allOtherCases -> Nothing

databaseUrlEnvVarName :: String
databaseUrlEnvVarName = "DATABASE_URL"

getDbSystemFromPrismaSchema :: Psl.Ast.Schema -> AS.Db.DbSystem
getDbSystemFromPrismaSchema prismaSchema =
  case getProviderFromPrismaSchema prismaSchema of
    -- We parse raw config block values from Prisma file,
    -- so we need match the provider names with quotes.
    Just "\"postgresql\"" -> AS.App.Db.PostgreSQL
    Just "\"sqlite\"" -> AS.App.Db.SQLite
    Just provider -> AS.App.Db.UnsupportedDbSystem provider
    Nothing -> AS.App.Db.MissingDbSystem

getProviderFromPrismaSchema :: Psl.Ast.Schema -> Maybe String
getProviderFromPrismaSchema =
  findPrismaConfigBlockKeyValue "provider"
    -- As per Prisma's docs there can be only ONE datasource block in the schema.
    -- But we are still handling the case where there are multiple datasource blocks.
    -- https://www.prisma.io/docs/orm/reference/prisma-schema-reference#remarks
    . concatMap (\(Psl.Ast.Datasource _ keyValues) -> keyValues)
    . Psl.Ast.getDatasources
