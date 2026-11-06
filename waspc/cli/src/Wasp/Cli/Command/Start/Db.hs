module Wasp.Cli.Command.Start.Db
  ( start,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.DevDb (startPostgresDevDb, throwCustomDatabaseInUseError)
import Wasp.Cli.DevDb.ArgumentsParser (DevDbOptions, devDbOptionsParser)
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import qualified Wasp.Message as Msg
import Wasp.Project.Db.Dev (DevDatabase (..), findDevDatabase)

-- | Starts a "managed" dev database, where "managed" means that
-- Wasp creates it and connects the Wasp app with it.
-- Wasp is smart while doing this so it checks which database is specified
-- in Wasp configuration and spins up a database of appropriate type.
start :: Arguments -> Command ()
start = withArguments startDbArgsParser $ \devDbOptions -> do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir

  let app = ASV.getApp appSpec

  liftIO (findDevDatabase appSpec) >>= \case
    CustomDatabase customDatabaseSource -> throwCustomDatabaseInUseError customDatabaseSource
    ManagedSqliteDatabase -> noteSQLiteDoesntNeedStart
    ManagedPostgresDatabase -> startPostgresDevDb waspProjectDir app.name devDbOptions
  where
    noteSQLiteDoesntNeedStart =
      cliSendMessageC . Msg.Info $
        "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."

startDbArgsParser :: ArgsParser DevDbOptions
startDbArgsParser = ArgsParser "wasp start db" devDbOptionsParser
