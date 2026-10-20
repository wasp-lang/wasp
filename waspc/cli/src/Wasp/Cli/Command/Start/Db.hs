module Wasp.Cli.Command.Start.Db
  ( start,
    withDevDb,
    DbStartOptions (..),
    dbStartOptionsParser,
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
import qualified Options.Applicative as Opt
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import System.IO.Error (ioeGetErrorString)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import qualified Wasp.Cli.Command.Db.Docker as Docker
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Port (resolvePort)
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import Wasp.Cli.Util.PortArgument (portParser)
import Wasp.Db.Postgres (defaultPostgresDockerImageSpec, defaultPostgresPort)
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

data DbStartOptions = DbStartOptions
  { dbPort :: Maybe PortNumber,
    dbImage :: Maybe DockerImageName,
    dbVolumeMountPath :: Maybe DockerVolumeMountPath
  }
  deriving (Eq, Show)

dbStartOptionsParser :: Opt.Parser DbStartOptions
dbStartOptionsParser =
  DbStartOptions
    <$> Opt.optional (portParser "db-port" ("Port to run the dev database on (default: " ++ show defaultPostgresPort ++ ")"))
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "db-image"
              <> Opt.metavar "IMAGE"
              <> Opt.help "Docker image to use for the database"
          )
      )
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "db-volume-mount-path"
              <> Opt.metavar "PATH"
              <> Opt.help "Path inside Docker container where database files are stored"
          )
      )

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

startPostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> Command ()
startPostgresDevDb waspProjectDir appName options =
  withPostgresSession waspProjectDir appName options StartNewDatabase $ \case
    StartedDatabase _ _ databaseJob -> do
      cliSendMessageC $ Msg.Info "PostgreSQL is running. Ctrl+C stops PostgreSQL."
      _ <- liftIO $ wait databaseJob
      E.throwError $ CommandError "PostgreSQL stopped" "The database container exited. Check the PostgreSQL logs above."
    ReusedDatabase _ ->
      E.throwError $ CommandError "Database not started" "No managed PostgreSQL database was started."

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
        withPostgresSession waspProjectDir (ASV.getApp appSpec).name options StartOrReuseDatabase $
          action . Just . getDatabaseForSession

data DatabaseStartPolicy = StartNewDatabase | StartOrReuseDatabase
  deriving (Eq)

data DatabaseSession
  = ReusedDatabase Dev.Postgres.DevDbSpec
  | StartedDatabase Dev.Postgres.DevDbSpec String (Async ExitCode)

withPostgresSession :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> DatabaseStartPolicy -> (DatabaseSession -> Command a) -> Command a
withPostgresSession waspProjectDir appName options policy action = do
  prepared <- preparePostgresDevDb waspProjectDir appName options policy
  bracket (acquire prepared) release use
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
          when (policy == StartNewDatabase) $ cliSendMessageC $ Msg.Info $ unlines $ additionalInfoLines db
        _ -> return ()
      action session

    release (StartedDatabase _ containerId databaseJob) =
      Docker.stopDatabase containerId `finally` (liftIO (cancel databaseJob) `finally` Docker.removeContainer containerId)
    release _ = return ()

data PreparedDatabase
  = ReuseDatabase DatabaseSession
  | CreateDatabase Dev.Postgres.DevDbSpec DockerImageName DockerVolumeMountPath

preparePostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DbStartOptions -> DatabaseStartPolicy -> Command PreparedDatabase
preparePostgresDevDb waspProjectDir appName options policy = do
  Docker.ensureDockerDaemonAvailable

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
      unless (null $ suppliedOptionNames options) $ cliSendMessageC $ Msg.Warning "Database options not used" (printf "These options only apply when starting a new container: %s. Stop the running database before changing them." (intercalate ", " (suppliedOptionNames options)))
      return $ ReusedDatabase devDbSpec

    prepareDbOnPort :: PortNumber -> Command PreparedDatabase
    prepareDbOnPort port = do
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
