module Wasp.Cli.DevDb
  ( startPostgresDevDb,
    withRunningPostgresDevDb,
    throwIfCustomDbAlreadyInUse,
  )
where

import Control.Concurrent.Async (async, cancel, race, wait)
import Control.Monad (when)
import Control.Monad.Catch (bracket)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode, exitFailure)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Common (throwIfExeIsNotAvailable)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.DevDb.ArgumentsParser (DevDbOptions (..))
import Wasp.Cli.Port (resolvePort)
import Wasp.Db.Postgres (defaultPostgresPort)
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import Wasp.Project.Env (dotEnvServer)
import Wasp.Util (checkExitSuccess)
import qualified Wasp.Util.Docker as Docker

startPostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DevDbOptions -> Command ()
startPostgresDevDb waspProjectDir appName devDbOptions =
  liftIO (Dev.Postgres.discoverProjectsRunningDevDb waspProjectDir appName) >>= \case
    Just runningDb -> noteDbIsAlreadyRunningAndExit runningDb
    Nothing ->
      withStartedPostgresDevDb waspProjectDir appName devDbOptions $ \waitForDbExit -> do
        cliSendMessageC $ Msg.Info "PostgreSQL dev database is ready. Press Ctrl+C to stop it."
        waitForDbExit
  where
    noteDbIsAlreadyRunningAndExit :: Dev.Postgres.DevDbSpec -> Command ()
    noteDbIsAlreadyRunningAndExit devDbSpec = do
      cliSendMessageC . Msg.Info . unlines $
        ("Your PostgreSQL dev database is already running on port " ++ show devDbSpec.port ++ ".")
          : additionalInfoLines devDbSpec
      liftIO exitFailure

-- | Runs the action with the project's PostgreSQL dev database: the one that's
-- already running, or a new one that is stopped and removed when the action
-- finishes.
withRunningPostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DevDbOptions -> Command a -> Command a
withRunningPostgresDevDb waspProjectDir appName devDbOptions action =
  liftIO (Dev.Postgres.discoverProjectsRunningDevDb waspProjectDir appName) >>= \case
    Just runningDb -> do
      throwUnlessDevDbAcceptsConnections =<< liftIO (Dev.Postgres.waitUntilDevDbAcceptsConnections runningDb)
      action
    Nothing -> withStartedPostgresDevDb waspProjectDir appName devDbOptions $ const action

-- | Starts the project's PostgreSQL dev database, and runs the action once the
-- database accepts connections. The action gets a way to wait for the database
-- to exit. The database is stopped and removed when the action finishes.
withStartedPostgresDevDb :: forall a. Path' Abs (Dir WaspProjectDir) -> String -> DevDbOptions -> (Command () -> Command a) -> Command a
withStartedPostgresDevDb waspProjectDir appName devDbOptions action = do
  throwIfDockerIsNotAvailable
  startDbOnPort =<< resolveDevDbPort
  where
    resolveDevDbPort :: Command PortNumber
    resolveDevDbPort =
      resolvePort
        devDbOptions.dbPort
        defaultPostgresPort
        []
        "Choose a different port with --db-port, or free up this one."
        "Free at least one of those ports by exiting the program listening on it, or choose the port yourself with --db-port."

    startDbOnPort :: PortNumber -> Command a
    startDbOnPort port = do
      let devDbSpec = Dev.Postgres.makeDevPostgresDbSpec waspProjectDir appName port
      cliSendMessageC . Msg.Info . unlines $
        "✨ Starting a PostgreSQL dev database (based on your Wasp config) ✨"
          : additionalInfoLines devDbSpec
            <> dockerRunInfoLines
      cliSendMessageC $ Msg.Info "..."
      -- Cancelling the database job stops the container, which `docker run --rm`
      -- then removes.
      bracket (liftIO $ async $ runDbJob devDbSpec) (liftIO . cancel) $ \dbJob -> do
        let waitForDbExit = do
              exitCode <- liftIO $ wait dbJob
              throwIfDbFailed exitCode
        liftIO (race (wait dbJob) (Dev.Postgres.waitUntilDevDbAcceptsConnections devDbSpec)) >>= \case
          Right connectionCheck -> do
            throwUnlessDevDbAcceptsConnections connectionCheck
            action waitForDbExit
          Left _exitCode -> throwDbError "The database stopped before accepting connections."

    runDbJob :: Dev.Postgres.DevDbSpec -> IO ExitCode
    runDbJob devDbSpec =
      Job.run
        $ Job.prefixWith Job.Db
        $ Dev.Postgres.runDevPostgresDb devDbSpec devDbOptions.dbImage devDbOptions.dbVolumeMountPath

    throwIfDbFailed :: ExitCode -> Command ()
    throwIfDbFailed exitCode =
      either throwDbError return $
        checkExitSuccess "Running the PostgreSQL dev database" exitCode

    -- These lines describe what `docker run` is about to use, so we print them
    -- only when starting the database: an already running container might have
    -- been started with a different image or mount path than the current
    -- invocation's arguments.
    dockerRunInfoLines :: [String]
    dockerRunInfoLines =
      [ " ℹ Using Docker image: " <> devDbOptions.dbImage,
        "   with the data volume mounted at: " <> devDbOptions.dbVolumeMountPath
      ]

throwUnlessDevDbAcceptsConnections :: Dev.Postgres.ConnectionCheck -> Command ()
throwUnlessDevDbAcceptsConnections = \case
  Dev.Postgres.AcceptsConnections -> return ()
  Dev.Postgres.UnableToCheckConnection ->
    throwDbError "Unable to check the database connection. Check that the database Docker image is a valid PostgreSQL image."

throwDbError :: String -> Command a
throwDbError = E.throwError . CommandError "Dev database failed"

throwIfDockerIsNotAvailable :: Command ()
throwIfDockerIsNotAvailable = do
  throwIfExeIsNotAvailable
    "docker"
    "To run PostgreSQL dev database, Wasp needs `docker` installed and in PATH."
  throwIfDockerDaemonIsNotReachable
  where
    throwIfDockerDaemonIsNotReachable :: Command ()
    throwIfDockerDaemonIsNotReachable =
      liftIO Docker.checkDockerDaemonConnection >>= \case
        Right () -> return ()
        Left dockerError ->
          E.throwError $
            CommandError
              "Couldn't connect to the Docker daemon"
              ( "To run PostgreSQL dev database, Wasp needs Docker to be running."
                  <> " Make sure Docker is started (e.g. by opening Docker Desktop) and try again.\n\n"
                  <> "Docker reported:\n"
                  <> dockerError
              )

additionalInfoLines :: Dev.Postgres.DevDbSpec -> [String]
additionalInfoLines devDbSpec =
  [ "",
    "Additional info:",
    " ℹ Connection URL, in case you might want to connect with external tools:",
    "     " <> Dev.Postgres.getDevConnectionUrl devDbSpec,
    " ℹ Database data is persisted in a Docker volume with the following name"
      <> " (useful to know if you will want to delete it at some point):",
    "     " <> devDbSpec.dockerVolumeName
  ]

throwIfCustomDbAlreadyInUse :: AS.AppSpec -> Command ()
throwIfCustomDbAlreadyInUse spec = do
  throwIfDbUrlInEnv
  throwIfDbUrlInServerDotEnv spec
  where
    throwIfDbUrlInEnv :: Command ()
    throwIfDbUrlInEnv = do
      dbUrl <- liftIO $ lookupEnv databaseUrlEnvVarName
      when (isJust dbUrl) $
        throwCustomDbAlreadyInUseError
          ( "Wasp has detected existing "
              <> databaseUrlEnvVarName
              <> " var in your environment.\n"
              <> "To have Wasp run the dev database for you, make sure you remove that env var first."
          )

    throwIfDbUrlInServerDotEnv :: AS.AppSpec -> Command ()
    throwIfDbUrlInServerDotEnv appSpec =
      when (isThereDbUrlInServerDotEnv appSpec) $
        throwCustomDbAlreadyInUseError
          ( printf
              ( "Wasp has detected that you have defined %s env var in your %s file.\n"
                  <> "To have Wasp run the dev database for you, make sure you remove that env var first."
              )
              databaseUrlEnvVarName
              (fromRelFile (dotEnvServer :: Path' (Rel WaspProjectDir) File'))
          )
      where
        isThereDbUrlInServerDotEnv = any ((== databaseUrlEnvVarName) . fst) . AS.devEnvVarsServer

    throwCustomDbAlreadyInUseError :: String -> Command ()
    throwCustomDbAlreadyInUseError msg =
      E.throwError $ CommandError "You are using custom database already" msg
