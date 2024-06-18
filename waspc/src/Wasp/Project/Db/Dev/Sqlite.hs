-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Sqlite
  ( defaultDevDbFile,
  )
where

defaultDevDbFile :: String
defaultDevDbFile = "file:./dev.db"
