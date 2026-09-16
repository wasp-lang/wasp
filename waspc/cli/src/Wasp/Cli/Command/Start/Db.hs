module Wasp.Cli.Command.Start.Db
  ( start,
  )
where

import Control.Monad (when)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Common (throwIfExeIsNotAvailable)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Port (findFirstFreeLocalPortInRange)
import Wasp.Cli.Util.Parser (withArguments)
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
start = withArguments "wasp start db" startDbArgsParser $ \args -> do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir

  throwIfCustomDbAlreadyInUse appSpec

  let (appName, _) = ASV.getApp appSpec

  case ASV.getValidDbSystem appSpec of
    AS.App.Db.SQLite -> noteSQLiteDoesntNeedStart
    AS.App.Db.PostgreSQL ->
      startPostgresDevDb
        waspProjectDir
        appName
        (dbImage args)
        (dbVolumeMountPath args)
  where
    noteSQLiteDoesntNeedStart =
      cliSendMessageC . Msg.Info $
        "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."

startDbArgsParser :: Opt.Parser StartDbArgs
startDbArgsParser =
  StartDbArgs
    <$> Opt.strOption
      ( Opt.long "db-image"
          <> Opt.metavar "IMAGE"
          <> Opt.help "Docker image to use for the database"
          <> Opt.showDefault
          <> Opt.value (fst defaultPostgresDockerImageSpec)
      )
    <*> Opt.strOption
      ( Opt.long "db-volume-mount-path"
          <> Opt.metavar "PATH"
          <> Opt.help "Path inside Docker container where database files are stored"
          <> Opt.showDefault
          <> Opt.value (snd defaultPostgresDockerImageSpec)
      )

data StartDbArgs = StartDbArgs
  { dbImage :: DockerImageName,
    dbVolumeMountPath :: DockerVolumeMountPath
  }

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

startPostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DockerImageName -> DockerVolumeMountPath -> Command ()
startPostgresDevDb waspProjectDir appName dbDockerImage dbDockerVolumeMountPath = do
  throwIfExeIsNotAvailable
    "docker"
    "To run PostgreSQL dev database, Wasp needs `docker` installed and in PATH."

  liftIO (Dev.Postgres.discoverProjectsRunningDevDb waspProjectDir appName) >>= \case
    Just runningDb -> noteDbIsAlreadyRunningAndExit runningDb
    Nothing -> startDbOnPort =<< findFreeDevDbPort
  where
    findFreeDevDbPort :: Command PortNumber
    findFreeDevDbPort =
      liftIO
        ( findFirstFreeLocalPortInRange
            defaultPostgresPort
            []
            "Free at least one of those ports by exiting the program listening on it."
        )
        >>= either throwNoFreePortError return

    throwNoFreePortError :: String -> Command a
    throwNoFreePortError = E.throwError . CommandError "No free port"

    noteDbIsAlreadyRunningAndExit :: Dev.Postgres.DevDbSpec -> Command ()
    noteDbIsAlreadyRunningAndExit devDbSpec = do
      cliSendMessageC . Msg.Info . unlines $
        ("Your PostgreSQL dev database is already running on port " ++ show devDbSpec.port ++ ".")
          : additionalInfoLines devDbSpec
      liftIO exitFailure

    startDbOnPort :: PortNumber -> Command ()
    startDbOnPort port = do
      let devDbSpec = Dev.Postgres.makeDevPostgresDbSpec waspProjectDir appName port
      cliSendMessageC . Msg.Info . unlines $
        "✨ Starting a PostgreSQL dev database (based on your Wasp config) ✨"
          : additionalInfoLines devDbSpec
            <> dockerRunInfoLines
      cliSendMessageC $ Msg.Info "..."
      liftIO $ Dev.Postgres.runDevPostgresDb devDbSpec dbDockerImage dbDockerVolumeMountPath

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

    -- These lines describe what `docker run` is about to use, so we print them
    -- only when starting the database: an already running container might have
    -- been started with a different image or mount path than the current
    -- invocation's arguments.
    dockerRunInfoLines :: [String]
    dockerRunInfoLines =
      [ " ℹ Using Docker image: " <> dbDockerImage,
        "   with the data volume mounted at: " <> dbDockerVolumeMountPath
      ]
