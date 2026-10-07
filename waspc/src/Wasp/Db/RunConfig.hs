module Wasp.Db.RunConfig
  ( DbRunConfig (..),
    DbConnection (..),
    DbUrlSource (..),
    connectionUrl,
    withConnectionUrl,
    resolveDevConnection,
  )
where

import Wasp.AppSpec.App.Db (DbSystem)
import qualified Wasp.Project.Db.Dev.Postgres as Postgres

data DbRunConfig = DbRunConfig
  { provider :: DbSystem,
    connection :: DbConnection
  }

data DbConnection
  = SuppliedConnection DbUrlSource String
  | SQLiteFile String
  | LocalPostgreSQL (Maybe Postgres.DevDbSpec)
  | Unconfigured

data DbUrlSource = Environment | ServerDotEnv | CommandOptions
  deriving (Eq, Show)

connectionUrl :: DbRunConfig -> Maybe String
connectionUrl config = case config.connection of
  SuppliedConnection _ url -> Just url
  SQLiteFile url -> Just url
  LocalPostgreSQL db -> Postgres.getDevConnectionUrl <$> db
  Unconfigured -> Nothing

withConnectionUrl :: DbUrlSource -> Maybe String -> DbRunConfig -> DbRunConfig
withConnectionUrl source maybeUrl config = case maybeUrl of
  Just url -> config {connection = SuppliedConnection source url}
  Nothing -> config

resolveDevConnection :: Maybe String -> Maybe String -> DbRunConfig -> DbRunConfig
resolveDevConnection environmentUrl serverDotEnvUrl =
  withConnectionUrl Environment environmentUrl
    . withConnectionUrl ServerDotEnv serverDotEnvUrl
