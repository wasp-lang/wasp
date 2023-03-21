module Wasp.Project.Db
  ( makeDevDatabaseUrl,
    databaseUrlEnvVarName,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Db.Dev.Postgres as DevPostgres

makeDevDatabaseUrl :: Path' Abs (Dir WaspProjectDir) -> [AS.Decl] -> Maybe String
makeDevDatabaseUrl waspProjectDir decls = do
  (appName, app) <- AS.getApp decls
  dbSystem <- AS.App.Db.system =<< AS.App.db app
  case dbSystem of
    AS.App.Db.SQLite -> Nothing
    AS.App.Db.PostgreSQL -> Just $ DevPostgres.makeDevConnectionUrl waspProjectDir appName

databaseUrlEnvVarName :: String
databaseUrlEnvVarName = "DATABASE_URL"
