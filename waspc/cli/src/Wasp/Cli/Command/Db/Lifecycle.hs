module Wasp.Cli.Command.Db.Lifecycle
  ( start,
    withManagedDb,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (Async, async, cancel, concurrently, race, wait)
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Control.Monad.Catch (bracket, bracketOnError, finally)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path')
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Error (ioeGetErrorString)
import System.Process (CreateProcess (create_group), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Common (throwIfExeIsNotAvailable)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Db.ArgumentsParser (StartDbArgs (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Port (resolvePort)
import Wasp.Db.Postgres (defaultPostgresDockerImageSpec, defaultPostgresPort)
import qualified Wasp.Job as Job
import Wasp.Job.IO.PrefixedWriter (printJobMessagePrefixed, runPrefixedWriter)
import Wasp.Job.Process (runProcessAsJob)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)

-- | Starts a "managed" dev database, where "managed" means that
-- Wasp creates it and connects the Wasp app with it.
-- Wasp is smart while doing this so it checks which database is specified
-- in Wasp configuration and spins up a database of appropriate type.
start :: StartDbArgs -> Command ()
start args =
  withDatabaseSession args StartNewDatabase $ \case
    NoDatabase ->
      cliSendMessageC $ Msg.Info "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."
    StartedDatabase _ _ process -> do
      cliSendMessageC $ Msg.Info "PostgreSQL is running. Ctrl+C stops PostgreSQL."
      _ <- liftIO $ wait process
      E.throwError $ CommandError "PostgreSQL stopped" "The database container exited. Check the PostgreSQL logs above."
    _ -> E.throwError $ CommandError "Database not started" "No managed PostgreSQL database was started."

printDbMessages :: Chan Job.JobMessage -> IO ()
printDbMessages channel = runPrefixedWriter go
  where
    go = do
      message <- liftIO $ readChan channel
      case Job._data message of
        Job.JobOutput output _ -> do
          printJobMessagePrefixed $ Job.JobMessage (Job.JobOutput output Job.Stdout) Job.Db
          go
        Job.JobExit _ -> return ()

withManagedDb :: StartDbArgs -> (Maybe Dev.Postgres.DevDbSpec -> Command a) -> Command a
withManagedDb args action = withDatabaseSession args EnsureDatabase (action . databaseForSession)

data DatabaseRequest = StartNewDatabase | EnsureDatabase
  deriving (Eq)

data DatabaseSession
  = NoDatabase
  | ReusedDatabase Dev.Postgres.DevDbSpec
  | StartedDatabase Dev.Postgres.DevDbSpec String (Async ExitCode)

data PreparedDatabase
  = ExistingDatabase DatabaseSession
  | NewDatabase Dev.Postgres.DevDbSpec DockerImageName DockerVolumeMountPath

databaseForSession :: DatabaseSession -> Maybe Dev.Postgres.DevDbSpec
databaseForSession NoDatabase = Nothing
databaseForSession (ReusedDatabase db) = Just db
databaseForSession (StartedDatabase db _ _) = Just db

withDatabaseSession :: StartDbArgs -> DatabaseRequest -> (DatabaseSession -> Command a) -> Command a
withDatabaseSession args request action = do
  prepared <- prepare
  bracket (acquire prepared) release use
  where
    use session = do
      case session of
        StartedDatabase db _ process -> do
          result <-
            withDatabaseError "Could not start PostgreSQL" $
              race (wait process) (Dev.Postgres.waitForReadyDevDb db)
          case result of
            Left _ -> E.throwError $ CommandError "Could not start PostgreSQL" "The Docker process exited before PostgreSQL was ready. Check the database output above."
            Right () -> return ()
          when (request == StartNewDatabase) $ do
            cliSendMessageC $ Msg.Info $ printf "Database URL: %s" (Dev.Postgres.getDevConnectionUrl db)
            cliSendMessageC $ Msg.Info $ printf "Data volume: %s" db.dockerVolumeName
        _ -> return ()
      action session
    prepare = do
      InWaspProject waspProjectDir <- require
      WaspSpecAvailable <- require
      appSpec <- analyze waspProjectDir
      customDb <- liftIO $ hasExternalDatabaseUrl appSpec
      if customDb
        then case request of
          StartNewDatabase -> E.throwError $ CommandError "No database to start" "DATABASE_URL points to an external database. Start that database outside Wasp."
          EnsureDatabase -> rejectUnusedOptions "DATABASE_URL is set." args >> return (ExistingDatabase NoDatabase)
        else case ASV.getValidDbSystem appSpec of
          AS.App.Db.SQLite -> case request of
            StartNewDatabase -> return (ExistingDatabase NoDatabase)
            EnsureDatabase -> rejectUnusedOptions "This project uses SQLite." args >> return (ExistingDatabase NoDatabase)
          AS.App.Db.PostgreSQL -> do
            let appName = (ASV.getApp appSpec).name
            preparePostgresDevDb waspProjectDir appName args request

    acquire (ExistingDatabase session) = return session
    acquire (NewDatabase db image mountPath) =
      bracketOnError
        (withDatabaseError "Could not start PostgreSQL" $ Dev.Postgres.createDevPostgresDb db image mountPath)
        removeContainer
        ( \containerId -> do
            process <- liftIO $ async $ do
              channel <- newChan
              fst
                <$> concurrently
                  (runProcessAsJob (proc "docker" ["start", "--attach", containerId]) Job.Db channel)
                  (printDbMessages channel)
            return $ StartedDatabase db containerId process
        )

    release (StartedDatabase _ containerId process) =
      stopDatabase containerId `finally` (liftIO (cancel process) `finally` removeContainer containerId)
    release _ = return ()

    stopDatabase containerId = do
      cliSendMessageC $ Msg.Start "Stopping database..."
      runDockerCleanup "stop" containerId

removeContainer :: String -> Command ()
removeContainer = runDockerCleanup "rm"

runDockerCleanup :: String -> String -> Command ()
runDockerCleanup operation containerId = do
  let args = [operation] ++ ["--force" | operation == "rm"] ++ [containerId]
  -- Let Docker finish stopping or removing the container even if the user presses Ctrl+C again.
  (status, _, errors) <- liftIO $ readCreateProcessWithExitCode ((proc "docker" args) {create_group = True}) ""
  when (status /= ExitSuccess && not ("No such container" `isInfixOf` errors)) $
    if operation == "stop"
      then cliSendMessageC $ Msg.Warning "Could not stop PostgreSQL" (printf "Wasp will try to remove the container. %s" errors)
      else cliSendMessageC $ Msg.Warning "Could not remove PostgreSQL container" (printf "Try `docker rm --force %s`. %s" containerId errors)

rejectUnusedOptions :: String -> StartDbArgs -> Command ()
rejectUnusedOptions source args =
  unless (null options)
    $ E.throwError
    $ CommandError
      "Database options do not apply"
      (printf "%s These options only apply to Wasp-managed PostgreSQL: %s. Remove them to continue." source (intercalate ", " options))
  where
    options = suppliedOptionNames args

suppliedOptionNames :: StartDbArgs -> [String]
suppliedOptionNames args =
  ["--db-port" | isJust args.dbPort]
    ++ ["--db-image" | isJust args.dbImage]
    ++ ["--db-volume-mount-path" | isJust args.dbVolumeMountPath]

requireDockerAvailable :: Command ()
requireDockerAvailable = do
  (exitCode, _, stderr) <- liftIO $ readProcessWithExitCode "docker" ["info", "--format", "{{.ServerVersion}}"] ""
  when (exitCode /= ExitSuccess)
    $ E.throwError
    $ CommandError "Docker unavailable" (printf "Start Docker and retry. %s" stderr)

preparePostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> StartDbArgs -> DatabaseRequest -> Command PreparedDatabase
preparePostgresDevDb waspProjectDir appName args request = do
  throwIfExeIsNotAvailable
    "docker"
    "To run PostgreSQL dev database, Wasp needs `docker` installed and in PATH."
  requireDockerAvailable

  liftIO (Dev.Postgres.discoverProjectsRunningDevDb waspProjectDir appName) >>= \case
    Just runningDb -> case request of
      StartNewDatabase ->
        E.throwError $
          CommandError
            "PostgreSQL already running"
            ( printf
                "This project's database is already running. Stop the command that started it, or run `docker stop %s` before starting it again. Database URL: %s"
                runningDb.dockerContainerName
                (Dev.Postgres.getDevConnectionUrl runningDb)
            )
      EnsureDatabase -> ExistingDatabase <$> reportReusedDb runningDb
    Nothing -> prepareDbOnPort =<< resolveDevDbPort
  where
    dbDockerImage = fromMaybe (fst defaultPostgresDockerImageSpec) args.dbImage
    dbDockerVolumeMountPath = fromMaybe (snd defaultPostgresDockerImageSpec) args.dbVolumeMountPath

    resolveDevDbPort :: Command PortNumber
    resolveDevDbPort =
      resolvePort
        args.dbPort
        defaultPostgresPort
        []
        "Choose a different port with --db-port, or free up this one."
        "Free at least one of those ports by exiting the program listening on it, or choose the port yourself with --db-port."

    reportReusedDb :: Dev.Postgres.DevDbSpec -> Command DatabaseSession
    reportReusedDb devDbSpec = do
      withDatabaseError "PostgreSQL is not ready" $ Dev.Postgres.waitForReadyDevDb devDbSpec
      cliSendMessageC $ Msg.Info "PostgreSQL was already running for this project. Wasp did not start it and will not stop it."
      unless (null $ suppliedOptionNames args)
        $ cliSendMessageC
        $ Msg.Warning
          "Database options not used"
          (printf "These options only apply when starting a new container: %s. Stop the running database before changing them." (intercalate ", " (suppliedOptionNames args)))
      return $ ReusedDatabase devDbSpec

    prepareDbOnPort :: PortNumber -> Command PreparedDatabase
    prepareDbOnPort port = do
      let devDbSpec = Dev.Postgres.makeDevPostgresDbSpec waspProjectDir appName port
      ensureImageAvailable
      cliSendMessageC $ Msg.Start "Starting PostgreSQL..."
      cliSendMessageC $ Msg.Info $ unlines dockerRunInfoLines
      return $ NewDatabase devDbSpec dbDockerImage dbDockerVolumeMountPath

    -- These lines describe what Docker is about to use, so we print them
    -- only when starting the database: an already running container might have
    -- been started with a different image or mount path than the current
    -- invocation's arguments.
    dockerRunInfoLines :: [String]
    dockerRunInfoLines =
      [ printf " ℹ Using Docker image: %s" dbDockerImage,
        printf "   with the data volume mounted at: %s" dbDockerVolumeMountPath
      ]

    ensureImageAvailable :: Command ()
    ensureImageAvailable = do
      (imageStatus, _, _) <- liftIO $ readProcessWithExitCode "docker" ["image", "inspect", dbDockerImage] ""
      when (imageStatus /= ExitSuccess) $ do
        cliSendMessageC $ Msg.Start $ printf "Pulling PostgreSQL image %s..." dbDockerImage
        pullStatus <- liftIO $ do
          channel <- newChan
          fst <$> concurrently (runProcessAsJob (proc "docker" ["pull", dbDockerImage]) Job.Db channel) (printDbMessages channel)
        when (pullStatus /= ExitSuccess)
          $ E.throwError
          $ CommandError "Could not pull PostgreSQL image" (printf "Check the image name, registry access, and network connection. Image: %s" dbDockerImage)

withDatabaseError :: String -> IO a -> Command a
withDatabaseError title action = do
  result <- liftIO $ try action
  case result of
    Left (err :: IOException) -> E.throwError $ CommandError title $ ioeGetErrorString err
    Right value -> return value

hasExternalDatabaseUrl :: AS.AppSpec -> IO Bool
hasExternalDatabaseUrl appSpec = do
  envUrl <- lookupEnv databaseUrlEnvVarName
  return $ isJust envUrl || any ((== databaseUrlEnvVarName) . fst) (AS.devEnvVarsServer appSpec)
