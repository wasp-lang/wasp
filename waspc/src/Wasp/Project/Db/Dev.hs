{-# LANGUAGE TupleSections #-}

-- | This module captures how Wasp finds the dev database it manages for an app.
module Wasp.Project.Db.Dev
  ( DevDatabase (..),
    CustomDatabaseSource (..),
    findDevDatabase,
    findDevDatabaseUrl,
    devDatabaseEnvVars,
  )
where

import Data.Maybe (maybeToList)
import System.Environment (lookupEnv)
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Env (EnvVar)
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.Project.Db.Dev.Postgres as DevPostgres
import qualified Wasp.Project.Db.Dev.Sqlite as DevSqlite

-- | The database the app uses in development.
data DevDatabase
  = -- | The user configured a database of their own, by setting the database URL.
    CustomDatabase CustomDatabaseSource
  | ManagedSqliteDatabase
  | ManagedPostgresDatabase

-- | Where the user set the database URL.
data CustomDatabaseSource = InEnvironment | InDotEnvServer

findDevDatabase :: AppSpec -> IO DevDatabase
findDevDatabase spec =
  lookupEnv databaseUrlEnvVarName >>= \case
    Just _ -> return $ CustomDatabase InEnvironment
    Nothing
      | isDbUrlInDotEnvServer -> return $ CustomDatabase InDotEnvServer
      | otherwise -> return $ case ASV.getValidDbSystem spec of
          AS.Db.SQLite -> ManagedSqliteDatabase
          AS.Db.PostgreSQL -> ManagedPostgresDatabase
  where
    isDbUrlInDotEnvServer = any ((== databaseUrlEnvVarName) . fst) spec.devEnvVarsServer

-- | Connection URL of the dev database Wasp manages for the app. 'Nothing' if
-- the user configured a database of their own, or if the managed database is
-- not running.
findDevDatabaseUrl :: AppSpec -> IO (Maybe String)
findDevDatabaseUrl spec =
  findDevDatabase spec >>= \case
    CustomDatabase _ -> return Nothing
    ManagedSqliteDatabase -> return $ Just DevSqlite.defaultDevDbFile
    ManagedPostgresDatabase ->
      fmap DevPostgres.getDevConnectionUrl
        <$> DevPostgres.discoverProjectsRunningDevDb spec.waspProjectDir (ASV.getApp spec).name

devDatabaseEnvVars :: Maybe String -> [EnvVar]
devDatabaseEnvVars devDatabaseUrl = maybeToList ((databaseUrlEnvVarName,) <$> devDatabaseUrl)
