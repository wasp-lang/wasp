module Wasp.Project.Db
  ( makeDevDatabaseUrl,
    databaseUrlEnvVarName,
    getDbSystemFromPrismaSchema,
    DbSystemParseError (..),
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.App.Db as AS.Db
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Db.Dev.Postgres as DevPostgres
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Util (findPrismaConfigBlockValueByKey)

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

data DbSystemParseError = UnsupportedDbSystem String | MissingDbSystem
  deriving (Eq, Show)

getDbSystemFromPrismaSchema :: Psl.Schema.Schema -> Either DbSystemParseError AS.Db.DbSystem
getDbSystemFromPrismaSchema prismaSchema =
  case getProviderFromPrismaSchema prismaSchema of
    -- We parse raw config block values from Prisma file,
    -- so we need match the provider names with quotes.
    Just "\"postgresql\"" -> Right AS.App.Db.PostgreSQL
    Just "\"sqlite\"" -> Right AS.App.Db.SQLite
    Just provider -> Left $ UnsupportedDbSystem provider
    Nothing -> Left MissingDbSystem

getProviderFromPrismaSchema :: Psl.Schema.Schema -> Maybe String
getProviderFromPrismaSchema =
  findPrismaConfigBlockValueByKey "provider"
    -- As per Prisma's docs there can be only ONE datasource block in the schema.
    -- But we are still handling the case where there are multiple datasource blocks.
    -- https://www.prisma.io/docs/orm/reference/prisma-schema-reference#remarks
    . concatMap (\(Psl.ConfigBlock.ConfigBlock _type _name keyValues) -> keyValues)
    . Psl.Schema.getDatasources
