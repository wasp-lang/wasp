module Wasp.Project.Db
  ( makeDevDatabaseUrl,
    databaseUrlEnvVarName,
    validDbUrlInPrismaSchema,
    validDbUrlExprForPrismaSchema,
    getDbSystemFromPrismaSchema,
    isDbUrlInPrismaSchemaValid,
    DbSystemParseError (..),
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.App.Db as AS.Db
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Db.Dev.Postgres as DevPostgres
import qualified Wasp.Project.Db.Dev.Sqlite as DevSqlite
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Generator.Argument (generateExpression)
import Wasp.Psl.Util (findPrismaConfigBlockKeyValuePair)

makeDevDatabaseUrl ::
  Path' Abs (Dir WaspProjectDir) ->
  AS.Db.DbSystem ->
  [AS.Decl] ->
  Maybe String
makeDevDatabaseUrl waspProjectDir dbSystem decls = do
  (appName, _) <- AS.getApp decls
  case dbSystem of
    AS.App.Db.PostgreSQL -> Just $ DevPostgres.makeDevConnectionUrl waspProjectDir appName
    AS.App.Db.SQLite -> Just DevSqlite.defaultDevDbFile

databaseUrlEnvVarName :: String
databaseUrlEnvVarName = "DATABASE_URL"

-- | Datasource block in Prisma schema should have a `url` key with a value of `env("DATABASE_URL")`.
-- This is validatd in Wasp.Psl.Valid where we check if the `url` key has the correct value.
validDbUrlExprForPrismaSchema :: Psl.Argument.Expression
validDbUrlExprForPrismaSchema = Psl.Argument.FuncExpr "env" [Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr databaseUrlEnvVarName]

validDbUrlInPrismaSchema :: String
validDbUrlInPrismaSchema = generateExpression validDbUrlExprForPrismaSchema

data DbSystemParseError = UnsupportedDbSystem String | MissingDbSystem
  deriving (Eq, Show)

getDbSystemFromPrismaSchema :: Psl.Schema.Schema -> Either DbSystemParseError AS.Db.DbSystem
getDbSystemFromPrismaSchema prismaSchema =
  case getDbProviderFromPrismaSchema prismaSchema of
    Just (Psl.Argument.StringExpr "postgresql") -> Right AS.App.Db.PostgreSQL
    Just (Psl.Argument.StringExpr "sqlite") -> Right AS.App.Db.SQLite
    Just (Psl.Argument.StringExpr provider) -> Left $ UnsupportedDbSystem provider
    Just anyOtherArgument -> Left $ UnsupportedDbSystem $ show anyOtherArgument
    Nothing -> Left MissingDbSystem

isDbUrlInPrismaSchemaValid :: Psl.Schema.Schema -> Bool
isDbUrlInPrismaSchemaValid =
  (Just validDbUrlExprForPrismaSchema ==)
    . getDbUrlFromPrismaSchema

getDbProviderFromPrismaSchema :: Psl.Schema.Schema -> Maybe Psl.Argument.Expression
getDbProviderFromPrismaSchema =
  findPrismaConfigBlockKeyValuePair "provider"
    . getAllDatasourceKeyValuePairs

getDbUrlFromPrismaSchema :: Psl.Schema.Schema -> Maybe Psl.Argument.Expression
getDbUrlFromPrismaSchema =
  findPrismaConfigBlockKeyValuePair "url"
    . getAllDatasourceKeyValuePairs

-- As per Prisma's docs there can be only ONE datasource block in the schema.
-- But we are still handling the case where there are multiple datasource blocks.
-- https://www.prisma.io/docs/orm/reference/prisma-schema-reference#remarks
getAllDatasourceKeyValuePairs :: Psl.Schema.Schema -> [Psl.ConfigBlock.KeyValuePair]
getAllDatasourceKeyValuePairs =
  concatMap (\(Psl.ConfigBlock.ConfigBlock _type _name keyValuePairs) -> keyValuePairs)
    . Psl.Schema.getDatasources
