module Wasp.Cli.Command.Db.DevDb
  ( withDevDb,
    withPostgresSession,
    DatabaseStartPolicy (..),
    DatabaseSession (..),
    DatabaseUrlSource (..),
    findDatabaseUrlSource,
  )
where

import Control.Concurrent.Async (Async, async, cancel, race, wait)
import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Control.Monad.Catch (bracket, bracketOnError, finally)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path')
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import System.IO.Error (ioeGetErrorString)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Compile (analyze)
import qualified Wasp.Cli.Command.Db.Docker as Docker
import Wasp.Cli.Command.Db.StartOptions (DbStartOptions (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Port (resolvePort)
import Wasp.Db.Postgres (defaultPostgresDockerImageSpec, defaultPostgresPort)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)

withDevDb :: DbStartOptions -> (Maybe Dev.Postgres.DevDbSpec -> Command a) -> Command a
withDevDb options action = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir
  databaseUrlSource <- liftIO $ findDatabaseUrlSource appSpec
  case databaseUrlSource of
    Just _ -> do
      throwIfDbStartOptionsSupplied "DATABASE_URL is set." options
      action Nothing
    Nothing -> case ASV.getValidDbSystem appSpec of
      AS.App.Db.SQLite -> do
        throwIfDbStartOptionsSupplied "This project uses SQLite." options
        action Nothing
      AS.App.Db.PostgreSQL ->
        withPostgresSession waspProjectDir (ASV.getApp appSpec).name options AllowExistingDatabase $
          action . Just . getDatabaseForSession

data DatabaseStartPolicy = RequireNewDatabase | AllowExistingDatabase
  deriving (Eq)

data DatabaseSession
  = ReusedDatabase Dev.Postgres.DevDbSpec
  | StartedDatabase Dev.Postgres.DevDbSpec String (Async ExitCode)

withPostgresSession :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> DatabaseStartPolicy -> (DatabaseSession -> Command a) -> Command a
withPostgresSession waspProjectDir appName options policy action = do
  startupPlan <- prepareDatabaseStartup waspProjectDir appName options policy
  bracket (acquire startupPlan) release use
  where
    acquire (ReuseDatabase session) = return session
    acquire (CreateDatabase db image mountPath) =
      bracketOnError
        (withDatabaseError "Could not start PostgreSQL" $ Dev.Postgres.createDevPostgresContainer db image mountPath)
        Docker.removeContainer
        ( \containerId -> do
            databaseJob <- liftIO $ async $ Docker.runDockerJobAndPrintOutput ["start", "--attach", containerId]
            return $ StartedDatabase db containerId databaseJob
        )

    use session = do
      case session of
        StartedDatabase db _ databaseJob -> do
          result <-
            withDatabaseError "Could not start PostgreSQL" $
              race (wait databaseJob) (Dev.Postgres.waitForDevDbReady db)
          case result of
            Left _ -> E.throwError $ CommandError "Could not start PostgreSQL" "The Docker process exited before PostgreSQL was ready. Check the database output above."
            Right () -> return ()
          when (policy == RequireNewDatabase) $ cliSendMessageC $ Msg.Info $ unlines $ additionalInfoLines db
        _ -> return ()
      action session

    release (StartedDatabase _ containerId databaseJob) =
      Docker.stopDatabase containerId `finally` (liftIO (cancel databaseJob) `finally` Docker.removeContainer containerId)
    release _ = return ()

data DatabaseStartupPlan
  = ReuseDatabase DatabaseSession
  | CreateDatabase Dev.Postgres.DevDbSpec DockerImageName DockerVolumeMountPath

prepareDatabaseStartup :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> DatabaseStartPolicy -> Command DatabaseStartupPlan
prepareDatabaseStartup waspProjectDir appName options policy = do
  Docker.ensureDockerDaemonAvailable

  liftIO (Dev.Postgres.discoverProjectsRunningDevDb waspProjectDir appName) >>= \case
    Just runningDb -> case policy of
      RequireNewDatabase ->
        E.throwError $
          CommandError
            "PostgreSQL already running"
            ( printf
                "This project's database is already running on port %s. Stop the command that started it, or run `docker stop %s` before starting it again.\n%s"
                (show runningDb.port)
                runningDb.dockerContainerName
                (unlines $ additionalInfoLines runningDb)
            )
      AllowExistingDatabase -> ReuseDatabase <$> prepareExistingDatabase runningDb
    Nothing -> prepareStartupOnPort =<< resolveDevDbPort
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
      unless (null $ suppliedOptionNames options) $ cliSendMessageC $ Msg.Warning "Database options not used" (printf "These options only apply when starting a new container: %s. Stop the running database before changing them." (intercalate ", " (suppliedOptionNames options)))
      return $ ReusedDatabase devDbSpec

    prepareStartupOnPort :: PortNumber -> Command DatabaseStartupPlan
    prepareStartupOnPort port = do
      let devDbSpec = Dev.Postgres.makeDevPostgresDbSpec waspProjectDir appName port
      Docker.ensureImageAvailable dbDockerImage
      cliSendMessageC $ Msg.Start "Starting PostgreSQL..."
      cliSendMessageC $ Msg.Info $ unlines dockerRunInfoLines
      return $ CreateDatabase devDbSpec dbDockerImage dbDockerVolumeMountPath

    -- These lines describe what Docker is about to use, so we print them
    -- only when starting the database: an already running container might have
    -- been started with a different image or mount path than the current
    -- invocation's arguments.
    dockerRunInfoLines :: [String]
    dockerRunInfoLines =
      [ " ℹ Using Docker image: " <> dbDockerImage,
        "   with the data volume mounted at: " <> dbDockerVolumeMountPath
      ]

getDatabaseForSession :: DatabaseSession -> Dev.Postgres.DevDbSpec
getDatabaseForSession (ReusedDatabase db) = db
getDatabaseForSession (StartedDatabase db _ _) = db

data DatabaseUrlSource = EnvironmentVariable | ServerDotEnvFile

findDatabaseUrlSource :: AS.AppSpec -> IO (Maybe DatabaseUrlSource)
findDatabaseUrlSource appSpec = do
  envUrl <- lookupEnv databaseUrlEnvVarName
  return $
    if isJust envUrl
      then Just EnvironmentVariable
      else
        if any ((== databaseUrlEnvVarName) . fst) appSpec.devEnvVarsServer
          then Just ServerDotEnvFile
          else Nothing

throwIfDbStartOptionsSupplied :: String -> DbStartOptions -> Command ()
throwIfDbStartOptionsSupplied reason options =
  unless (null optionNames) $ E.throwError $ CommandError "Database options do not apply" (printf "%s These options only apply to Wasp-managed PostgreSQL: %s. Remove them to continue." reason (intercalate ", " optionNames))
  where
    optionNames = suppliedOptionNames options

suppliedOptionNames :: DbStartOptions -> [String]
suppliedOptionNames options =
  ["--db-port" | isJust options.dbPort]
    ++ ["--db-image" | isJust options.dbImage]
    ++ ["--db-volume-mount-path" | isJust options.dbVolumeMountPath]

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

withDatabaseError :: String -> IO a -> Command a
withDatabaseError title action = do
  result <- liftIO $ try action
  case result of
    Left (err :: IOException) -> E.throwError $ CommandError title $ ioeGetErrorString err
    Right value -> return value
