module Wasp.Cli.Command.Db.Cli (runDbCommand) where

import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.Call
  ( DbArgs (DbMigrateDev, DbReset, DbSeed, DbStart, DbStudio),
  )
import Wasp.Cli.Command.Db (runAsDbCommand)
import Wasp.Cli.Command.Db.Migrate (migrateDev)
import Wasp.Cli.Command.Db.Reset (reset)
import Wasp.Cli.Command.Db.Seed (seed)
import Wasp.Cli.Command.Db.Studio (studio)
import Wasp.Cli.Command.Start.Db (start)

runDbCommand :: DbArgs -> IO ()
runDbCommand args = case args of
  DbStart -> runCommand start
  DbMigrateDev migrateArgs -> runAsDbCommand $ migrateDev migrateArgs
  DbReset -> runAsDbCommand reset
  DbSeed seedName -> runAsDbCommand $ seed seedName
  DbStudio -> runAsDbCommand studio
