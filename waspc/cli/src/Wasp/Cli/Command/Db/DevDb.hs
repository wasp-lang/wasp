module Wasp.Cli.Command.Db.DevDb
  ( start,
    withDevDb,
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
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
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
import Wasp.Cli.Command.Db.StartOptions (DbStartOptions (..))
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
import Wasp.Project.Env (dotEnvServer)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)

-- | Starts a "managed" dev database, where "managed" means that
-- Wasp creates it and connects the Wasp app with it.
-- Wasp is smart while doing this so it checks which database is specified
-- in Wasp configuration and spins up a database of appropriate type.
start :: DbStartOptions -> Command ()
start options = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir
  databaseUrlSource <- liftIO $ findDatabaseUrlSource appSpec
  case databaseUrlSource of
    Just source -> throwCustomDatabaseError source
    Nothing -> case ASV.getValidDbSystem appSpec of
      AS.App.Db.SQLite ->
        cliSendMessageC $ Msg.Info "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."
      AS.App.Db.PostgreSQL ->
        withPostgresSession waspProjectDir (ASV.getApp appSpec).name options StartNewDatabase $ \case
          StartedDatabase _ _ databaseJob -> do
            cliSendMessageC $ Msg.Info "PostgreSQL is running. Ctrl+C stops PostgreSQL."
            _ <- liftIO $ wait databaseJob
            E.throwError $ CommandError "PostgreSQL stopped" "The database container exited. Check the PostgreSQL logs above."
          ReusedDatabase _ ->
            E.throwError $ CommandError "Database not started" "No managed PostgreSQL database was started."

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

withDevDb :: DbStartOptions -> (Maybe Dev.Postgres.DevDbSpec -> Command a) -> Command a
withDevDb options action = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir
  databaseUrlSource <- liftIO $ findDatabaseUrlSource appSpec
  case databaseUrlSource of
    Just _ -> do
      rejectUnusedOptions "DATABASE_URL is set." options
      action Nothing
    Nothing -> case ASV.getValidDbSystem appSpec of
      AS.App.Db.SQLite -> do
        rejectUnusedOptions "This project uses SQLite." options
        action Nothing
      AS.App.Db.PostgreSQL ->
        withPostgresSession waspProjectDir (ASV.getApp appSpec).name options StartOrReuseDatabase $
          action . Just . getDatabaseForSession

data DatabaseStartPolicy = StartNewDatabase | StartOrReuseDatabase
  deriving (Eq)

data DatabaseSession
  = ReusedDatabase Dev.Postgres.DevDbSpec
  | StartedDatabase Dev.Postgres.DevDbSpec String (Async ExitCode)

data PreparedDatabase
  = ReuseDatabase DatabaseSession
  | CreateDatabase Dev.Postgres.DevDbSpec DockerImageName DockerVolumeMountPath

getDatabaseForSession :: DatabaseSession -> Dev.Postgres.DevDbSpec
getDatabaseForSession (ReusedDatabase db) = db
getDatabaseForSession (StartedDatabase db _ _) = db

withPostgresSession :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> DatabaseStartPolicy -> (DatabaseSession -> Command a) -> Command a
withPostgresSession waspProjectDir appName options policy action = do
  prepared <- preparePostgresDevDb waspProjectDir appName options policy
  bracket (acquire prepared) release use
  where
    use session = do
      case session of
        StartedDatabase db _ databaseJob -> do
          result <-
            withDatabaseError "Could not start PostgreSQL" $
              race (wait databaseJob) (Dev.Postgres.waitForDevDbReady db)
          case result of
            Left _ -> E.throwError $ CommandError "Could not start PostgreSQL" "The Docker process exited before PostgreSQL was ready. Check the database output above."
            Right () -> return ()
          when (policy == StartNewDatabase)
            $ cliSendMessageC
            $ Msg.Info
            $ unlines
            $ additionalInfoLines db
        _ -> return ()
      action session
    acquire (ReuseDatabase session) = return session
    acquire (CreateDatabase db image mountPath) =
      bracketOnError
        (withDatabaseError "Could not start PostgreSQL" $ Dev.Postgres.createDevPostgresContainer db image mountPath)
        removeContainer
        ( \containerId -> do
            databaseJob <- liftIO $ async $ do
              channel <- newChan
              fst
                <$> concurrently
                  (runProcessAsJob (proc "docker" ["start", "--attach", containerId]) Job.Db channel)
                  (printDbMessages channel)
            return $ StartedDatabase db containerId databaseJob
        )

    release (StartedDatabase _ containerId databaseJob) =
      stopDatabase containerId `finally` (liftIO (cancel databaseJob) `finally` removeContainer containerId)
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

rejectUnusedOptions :: String -> DbStartOptions -> Command ()
rejectUnusedOptions reason options =
  unless (null optionNames)
    $ E.throwError
    $ CommandError
      "Database options do not apply"
      (printf "%s These options only apply to Wasp-managed PostgreSQL: %s. Remove them to continue." reason (intercalate ", " optionNames))
  where
    optionNames = suppliedOptionNames options

suppliedOptionNames :: DbStartOptions -> [String]
suppliedOptionNames options =
  ["--db-port" | isJust options.dbPort]
    ++ ["--db-image" | isJust options.dbImage]
    ++ ["--db-volume-mount-path" | isJust options.dbVolumeMountPath]

requireDockerAvailable :: Command ()
requireDockerAvailable = do
  (exitCode, _, stderr) <- liftIO $ readProcessWithExitCode "docker" ["info", "--format", "{{.ServerVersion}}"] ""
  when (exitCode /= ExitSuccess)
    $ E.throwError
    $ CommandError "Docker unavailable" (printf "Start Docker and retry. %s" stderr)

preparePostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> DatabaseStartPolicy -> Command PreparedDatabase
preparePostgresDevDb waspProjectDir appName options policy = do
  throwIfExeIsNotAvailable
    "docker"
    "To run PostgreSQL dev database, Wasp needs `docker` installed and in PATH."
  requireDockerAvailable

  liftIO (Dev.Postgres.discoverProjectsRunningDevDb waspProjectDir appName) >>= \case
    Just runningDb -> case policy of
      StartNewDatabase ->
        E.throwError $
          CommandError
            "PostgreSQL already running"
            ( printf
                "This project's database is already running on port %s. Stop the command that started it, or run `docker stop %s` before starting it again.\n%s"
                (show runningDb.port)
                runningDb.dockerContainerName
                (unlines $ additionalInfoLines runningDb)
            )
      StartOrReuseDatabase -> ReuseDatabase <$> prepareExistingDatabase runningDb
    Nothing -> prepareDbOnPort =<< resolveDevDbPort
  where
    dbDockerImage = fromMaybe (fst defaultPostgresDockerImageSpec) options.dbImage
    dbDockerVolumeMountPath = fromMaybe (snd defaultPostgresDockerImageSpec) options.dbVolumeMountPath

    resolveDevDbPort :: Command PortNumber
    resolveDevDbPort =
      resolvePort
        options.dbPort
        defaultPostgresPort
        []
        "Choose a different port with --db-port, or free up this one."
        "Free at least one of those ports by exiting the program listening on it, or choose the port yourself with --db-port."

    prepareExistingDatabase :: Dev.Postgres.DevDbSpec -> Command DatabaseSession
    prepareExistingDatabase devDbSpec = do
      withDatabaseError "PostgreSQL is not ready" $ Dev.Postgres.waitForDevDbReady devDbSpec
      cliSendMessageC $ Msg.Info "PostgreSQL was already running for this project. Wasp did not start it and will not stop it."
      unless (null $ suppliedOptionNames options)
        $ cliSendMessageC
        $ Msg.Warning
          "Database options not used"
          (printf "These options only apply when starting a new container: %s. Stop the running database before changing them." (intercalate ", " (suppliedOptionNames options)))
      return $ ReusedDatabase devDbSpec

    prepareDbOnPort :: PortNumber -> Command PreparedDatabase
    prepareDbOnPort port = do
      let devDbSpec = Dev.Postgres.makeDevPostgresDbSpec waspProjectDir appName port
      ensureImageAvailable
      cliSendMessageC $ Msg.Start "Starting PostgreSQL..."
      cliSendMessageC $ Msg.Info $ unlines dockerRunInfoLines
      return $ CreateDatabase devDbSpec dbDockerImage dbDockerVolumeMountPath

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

additionalInfoLines :: Dev.Postgres.DevDbSpec -> [String]
additionalInfoLines db =
  [ "",
    "Additional info:",
    " ℹ Connection URL, in case you might want to connect with external tools:",
    printf "     %s" (Dev.Postgres.getDevConnectionUrl db),
    " ℹ Database data is persisted in a Docker volume with the following name",
    "   (useful to know if you will want to delete it at some point):",
    printf "     %s" db.dockerVolumeName
  ]

data DatabaseUrlSource = Environment | ServerDotEnv

findDatabaseUrlSource :: AS.AppSpec -> IO (Maybe DatabaseUrlSource)
findDatabaseUrlSource appSpec = do
  envUrl <- lookupEnv databaseUrlEnvVarName
  return $
    if isJust envUrl
      then Just Environment
      else
        if any ((== databaseUrlEnvVarName) . fst) appSpec.devEnvVarsServer
          then Just ServerDotEnv
          else Nothing

throwCustomDatabaseError :: DatabaseUrlSource -> Command a
throwCustomDatabaseError source =
  E.throwError $ CommandError "You are using custom database already" message
  where
    message = case source of
      Environment ->
        printf
          "Wasp has detected existing %s var in your environment.\nTo have Wasp run the dev database for you, make sure you remove that env var first."
          databaseUrlEnvVarName
      ServerDotEnv ->
        printf
          "Wasp has detected that you have defined %s env var in your %s file.\nTo have Wasp run the dev database for you, make sure you remove that env var first."
          databaseUrlEnvVarName
          (fromRelFile (dotEnvServer :: Path' (Rel WaspProjectDir) File'))
