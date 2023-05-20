module Wasp.Cli.Command.Db.Cli (dbCli) where

import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.Call
  ( DbArgs (DbMigrateDev, DbReset, DbSeed, DbStart, DbStudio),
  )
import Wasp.Cli.Command.Db (runDbCommand)
import Wasp.Cli.Command.Db.Migrate (migrateDev)
import Wasp.Cli.Command.Db.Reset (reset)
import Wasp.Cli.Command.Db.Seed (seed)
import Wasp.Cli.Command.Db.Studio (studio)
import Wasp.Cli.Command.Start.Db (start)

dbCli :: DbArgs -> IO ()
dbCli args = case args of
  DbStart -> runCommand start
  DbMigrateDev migrateArgs -> runDbCommand $ migrateDev migrateArgs
  DbReset -> runDbCommand reset
  DbSeed seedName -> runDbCommand $ seed seedName
  DbStudio -> runDbCommand studio
