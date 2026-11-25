module Wasp.Cli.DevDb
  ( startPostgresDevDb,
    withRunningPostgresDevDb,
    throwCustomDatabaseInUseError,
  )
where

import Control.Concurrent.Async (async, cancel, race, wait)
import Control.Monad.Catch (bracket)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
import System.Exit (ExitCode)
import Text.Printf (printf)
import Wasp.Cli.Command (Command, commandError, getConsole)
import Wasp.Cli.Command.Common (throwIfExeIsNotAvailable)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.DevDb.ArgumentsParser (DevDbOptions (..))
import Wasp.Cli.Port (resolvePort)
import Wasp.Db.Postgres (defaultPostgresPort)
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Project.Db.Dev (CustomDatabaseSource (..))
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import Wasp.Project.Env (dotEnvServer)
import Wasp.Util (checkExitSuccess, indent)
import qualified Wasp.Util.Docker as Docker
import Wasp.Util.IO.Process (WriteOutput)
import Wasp.Util.TextStyle (styleCode, styleLiteral)

startPostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DevDbOptions -> Command ()
startPostgresDevDb waspProjectDir appName devDbOptions =
  liftIO (Dev.Postgres.discoverProjectsRunningDevDb waspProjectDir appName) >>= \case
    Just runningDb -> throwDbIsAlreadyRunningError runningDb
    Nothing ->
      withStartedPostgresDevDb waspProjectDir appName devDbOptions $ \waitForDbExit -> do
        cliSendMessageC $ Msg.Info "The dev database is ready. Press Ctrl+C to stop it."
        waitForDbExit
  where
    throwDbIsAlreadyRunningError :: Dev.Postgres.DevDbSpec -> Command ()
    throwDbIsAlreadyRunningError devDbSpec =
      E.throwError
        $ commandError ("The dev database is already running on port " ++ styleLiteral (show devDbSpec.port))
        $ showFacts
        $ connectionFacts devDbSpec

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
        ("Choose a different port with " ++ styleCode "--db-port" ++ ", or free up this one.")
        ("Free at least one of those ports by exiting the program listening on it, or choose the port yourself with " ++ styleCode "--db-port" ++ ".")

    startDbOnPort :: PortNumber -> Command a
    startDbOnPort port = do
      console <- getConsole
      let devDbSpec = Dev.Postgres.makeDevPostgresDbSpec waspProjectDir appName port
      cliSendMessageC $ Msg.Info "Starting the dev database..."
      cliSendMessageC $ Msg.Info $ indent 2 $ showFacts $ connectionFacts devDbSpec ++ dockerRunFacts
      -- Cancelling the database job stops the container, which `docker run --rm`
      -- then removes.
      bracket (liftIO $ async $ runDbJob console.writeRawOutput devDbSpec) (liftIO . cancel) $ \dbJob -> do
        let waitForDbExit = do
              exitCode <- liftIO $ wait dbJob
              throwIfDbFailed exitCode
        liftIO (race (wait dbJob) (Dev.Postgres.waitUntilDevDbAcceptsConnections devDbSpec)) >>= \case
          Right connectionCheck -> do
            throwUnlessDevDbAcceptsConnections connectionCheck
            action waitForDbExit
          Left _exitCode -> throwDbError "The database stopped before accepting connections."

    runDbJob :: WriteOutput -> Dev.Postgres.DevDbSpec -> IO ExitCode
    runDbJob writeOutput devDbSpec =
      Job.run writeOutput
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
    dockerRunFacts :: [(String, String)]
    dockerRunFacts =
      [ ("Docker image", devDbOptions.dbImage),
        ("Volume mount path", devDbOptions.dbVolumeMountPath)
      ]

throwUnlessDevDbAcceptsConnections :: Dev.Postgres.ConnectionCheck -> Command ()
throwUnlessDevDbAcceptsConnections = \case
  Dev.Postgres.AcceptsConnections -> return ()
  Dev.Postgres.UnableToCheckConnection ->
    throwDbError "Unable to check the database connection. Check that the database Docker image is a valid PostgreSQL image."

throwDbError :: String -> Command a
throwDbError = E.throwError . commandError "Dev database failed"

throwIfDockerIsNotAvailable :: Command ()
throwIfDockerIsNotAvailable = do
  throwIfExeIsNotAvailable
    "docker"
    ("To run PostgreSQL dev database, Wasp needs " ++ styleCode "docker" ++ " installed and in PATH.")
  throwIfDockerDaemonIsNotReachable
  where
    throwIfDockerDaemonIsNotReachable :: Command ()
    throwIfDockerDaemonIsNotReachable =
      liftIO Docker.checkDockerDaemonConnection >>= \case
        Right () -> return ()
        Left dockerError ->
          E.throwError $
            commandError
              "Couldn't connect to the Docker daemon"
              ( "To run PostgreSQL dev database, Wasp needs Docker to be running."
                  <> " Make sure Docker is started (e.g. by opening Docker Desktop) and try again.\n\n"
                  <> "Docker reported:\n"
                  <> dockerError
              )

connectionFacts :: Dev.Postgres.DevDbSpec -> [(String, String)]
connectionFacts devDbSpec =
  [ ("Connection URL", Dev.Postgres.getDevConnectionUrl devDbSpec),
    ("Docker volume", devDbSpec.dockerVolumeName)
  ]

-- | Shows each fact on its own line, as "name: value", with the values lined
-- up.
showFacts :: [(String, String)] -> String
showFacts facts = intercalate "\n" $ map showFact facts
  where
    showFact (name, value) = padToLength labelLength (name ++ ":") ++ " " ++ styleLiteral value
    padToLength targetLength text = text ++ replicate (targetLength - length text) ' '
    labelLength = maximum $ map ((+ 1) . length . fst) facts

throwCustomDatabaseInUseError :: CustomDatabaseSource -> Command a
throwCustomDatabaseInUseError =
  E.throwError . commandError "A custom database is already configured" . \case
    InEnvironment ->
      "Wasp found "
        <> styleLiteral databaseUrlEnvVarName
        <> " in your environment.\n"
        <> "To have Wasp run the dev database for you, remove that env var first."
    InDotEnvServer ->
      printf
        ( "Wasp found %s in your %s file.\n"
            <> "To have Wasp run the dev database for you, remove that env var first."
        )
        (styleLiteral databaseUrlEnvVarName)
        (styleLiteral $ fromRelFile (dotEnvServer :: Path' (Rel WaspProjectDir) File'))
