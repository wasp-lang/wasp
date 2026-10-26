-- | This module captures how Wasp finds the dev database it manages for an app.
module Wasp.Project.Db.Dev
  ( findDevDatabaseUrl,
  )
where

import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.Project.Db.Dev.Postgres as DevPostgres
import qualified Wasp.Project.Db.Dev.Sqlite as DevSqlite

-- | Connection URL of the dev database Wasp manages for the app. 'Nothing' if
-- the user configured a database of their own, or if the managed database is
-- not running.
findDevDatabaseUrl :: AppSpec -> IO (Maybe String)
findDevDatabaseUrl spec = do
  isDbUrlInEnvironment <- isJust <$> lookupEnv databaseUrlEnvVarName
  if isDbUrlInEnvironment || isDbUrlInDotEnvServer
    then return Nothing
    else findManagedDevDatabaseUrl
  where
    isDbUrlInDotEnvServer = any ((== databaseUrlEnvVarName) . fst) spec.devEnvVarsServer

    findManagedDevDatabaseUrl = case ASV.getValidDbSystem spec of
      AS.Db.SQLite -> return $ Just DevSqlite.defaultDevDbFile
      AS.Db.PostgreSQL ->
        fmap DevPostgres.getDevConnectionUrl
          <$> DevPostgres.discoverProjectsRunningDevDb spec.waspProjectDir (ASV.getApp spec).name
