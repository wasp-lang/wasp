module Wasp.Cli.Command.Start.Db
  ( start,
  )
where

import Control.Concurrent.Async (wait)
import Control.Monad (forM_)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Db.DevDb
  ( DatabaseSession (..),
    DatabaseStartPolicy (..),
    DatabaseUrlSource (..),
    findDatabaseUrlSource,
    withPostgresSession,
  )
import Wasp.Cli.Command.Db.StartOptions (DbStartOptions, dbStartOptionsParser)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Project.Env (dotEnvServer)

-- | Starts a "managed" dev database, where "managed" means that
-- Wasp creates it and connects the Wasp app with it.
-- Wasp is smart while doing this so it checks which database is specified
-- in Wasp configuration and spins up a database of appropriate type.
start :: Arguments -> Command ()
start = withArguments startDbArgsParser $ \args -> do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir

  throwIfCustomDbAlreadyInUse appSpec

  let app = ASV.getApp appSpec

  case ASV.getValidDbSystem appSpec of
    AS.App.Db.SQLite -> noteSQLiteDoesntNeedStart
    AS.App.Db.PostgreSQL ->
      startPostgresDevDb
        waspProjectDir
        app.name
        args
  where
    noteSQLiteDoesntNeedStart =
      cliSendMessageC . Msg.Info $
        "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."

startDbArgsParser :: ArgsParser DbStartOptions
startDbArgsParser = ArgsParser "wasp start db" dbStartOptionsParser

throwIfCustomDbAlreadyInUse :: AS.AppSpec -> Command ()
throwIfCustomDbAlreadyInUse appSpec = do
  databaseUrlSource <- liftIO $ findDatabaseUrlSource appSpec
  forM_ databaseUrlSource $ \source ->
    E.throwError $ CommandError "You are using custom database already" (message source)
  where
    message source = case source of
      EnvironmentVariable ->
        printf
          "Wasp has detected existing %s var in your environment.\nTo have Wasp run the dev database for you, make sure you remove that env var first."
          databaseUrlEnvVarName
      ServerDotEnvFile ->
        printf
          "Wasp has detected that you have defined %s env var in your %s file.\nTo have Wasp run the dev database for you, make sure you remove that env var first."
          databaseUrlEnvVarName
          (fromRelFile (dotEnvServer :: Path' (Rel WaspProjectDir) File'))

startPostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> Command ()
startPostgresDevDb waspProjectDir appName options =
  withPostgresSession waspProjectDir appName options RequireNewDatabase $ \case
    StartedDatabase _ _ databaseJob -> do
      cliSendMessageC $ Msg.Info "PostgreSQL is running. Ctrl+C stops PostgreSQL."
      _ <- liftIO $ wait databaseJob
      E.throwError $ CommandError "PostgreSQL stopped" "The database container exited. Check the PostgreSQL logs above."
    ReusedDatabase _ ->
      E.throwError $ CommandError "Database not started" "No managed PostgreSQL database was started."
