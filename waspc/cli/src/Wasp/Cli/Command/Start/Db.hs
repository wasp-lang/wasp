module Wasp.Cli.Command.Start.Db
  ( start,
    getDbSystem,
    waspDevDbDockerVolumePrefix,
  )
where

import Control.Monad (when)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, isJust)
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
import System.Environment (lookupEnv)
import System.Process (callCommand)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd, throwIfExeIsNotAvailable)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Message as Msg
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Project.Db.Dev (makeDevDbUniqueId)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import Wasp.Project.Env (dotEnvServer)
import Wasp.Util (whenM)
import qualified Wasp.Util.Network.Socket as Socket

-- | Starts a "managed" dev database, where "managed" means that
-- Wasp creates it and connects the Wasp app with it.
-- Wasp is smart while doing this so it checks which database is specified
-- in Wasp configuration and spins up a database of appropriate type.
start :: Command ()
start = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  appSpec <- analyze waspProjectDir

  throwIfCustomDbAlreadyInUse appSpec

  let (appName, app) = ASV.getApp appSpec
  case getDbSystem app of
    AS.App.Db.SQLite -> noteSQLiteDoesntNeedStart
    AS.App.Db.PostgreSQL -> startPostgreDevDb waspProjectDir appName
  where
    noteSQLiteDoesntNeedStart =
      cliSendMessageC . Msg.Info $
        "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."

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
          ( "Wasp has detected existing " <> databaseUrlEnvVarName <> " var in your environment.\n"
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

getDbSystem :: AS.App.App -> AS.App.Db.DbSystem
getDbSystem app =
  fromMaybe AS.App.Db.SQLite (AS.App.db app >>= AS.App.Db.system)

startPostgreDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> Command ()
startPostgreDevDb waspProjectDir appName = do
  throwIfExeIsNotAvailable
    "docker"
    "To run PostgreSQL dev database, Wasp needs `docker` installed and in PATH."
  throwIfDevDbPortIsAlreadyInUse

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting a PostgreSQL dev database (based on your Wasp config) ✨",
        "",
        "Additional info:",
        " ℹ Connection URL, in case you might want to connect with external tools:",
        "     " <> connectionUrl,
        " ℹ Database data is persisted in a docker volume with the following name"
          <> " (useful to know if you will want to delete it at some point):",
        "     " <> dockerVolumeName
      ]

  -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
  --   only when initializing the database -> if it already exists, they will be ignored.
  --   This is how the postgres Docker image works.
  let command =
        printf
          ( unwords
              [ "docker run",
                "--name %s",
                "--rm",
                "--publish %d:5432",
                "-v %s:/var/lib/postgresql/data",
                "--env POSTGRES_PASSWORD=%s",
                "--env POSTGRES_USER=%s",
                "--env POSTGRES_DB=%s",
                "postgres"
              ]
          )
          dockerContainerName
          Dev.Postgres.defaultDevPort
          dockerVolumeName
          Dev.Postgres.defaultDevPass
          Dev.Postgres.defaultDevUser
          dbName
  liftIO $ callCommand command
  where
    dockerVolumeName = makeWaspDevDbDockerVolumeName waspProjectDir appName
    dockerContainerName = makeWaspDevDbDockerContainerName waspProjectDir appName
    dbName = Dev.Postgres.makeDevDbName waspProjectDir appName
    connectionUrl = Dev.Postgres.makeDevConnectionUrl waspProjectDir appName

    throwIfDevDbPortIsAlreadyInUse :: Command ()
    throwIfDevDbPortIsAlreadyInUse = do
      -- I am checking both conditions because of Docker having virtual network on Mac which
      -- always gives precedence to native ports so checking only if we can open the port is
      -- not enough because we can open it even if Docker container is already bound to that port.
      whenM (liftIO $ Socket.checkIfPortIsInUse devDbSocketAddress) throwPortAlreadyInUseError
      whenM (liftIO $ Socket.checkIfPortIsAcceptingConnections devDbSocketAddress) throwPortAlreadyInUseError
      where
        devDbSocketAddress = Socket.makeLocalHostSocketAddress $ fromIntegral Dev.Postgres.defaultDevPort
        throwPortAlreadyInUseError =
          E.throwError $
            CommandError
              "Port already in use"
              ( printf
                  "Wasp can't run PostgreSQL dev database for you since port %d is already in use."
                  Dev.Postgres.defaultDevPort
              )

-- | Docker volume name unique for the Wasp project with specified path and name.
makeWaspDevDbDockerVolumeName :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeWaspDevDbDockerVolumeName waspProjectDir appName =
  take maxDockerVolumeNameLength $
    waspDevDbDockerVolumePrefix <> "-" <> makeDevDbUniqueId waspProjectDir appName

waspDevDbDockerVolumePrefix :: String
waspDevDbDockerVolumePrefix = "wasp-dev-db"

maxDockerVolumeNameLength :: Int
maxDockerVolumeNameLength = 255

-- | Docker container name unique for the Wasp project with specified path and name.
makeWaspDevDbDockerContainerName :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeWaspDevDbDockerContainerName waspProjectDir appName =
  take maxDockerContainerNameLength $
    waspDevDbDockerVolumePrefix <> "-" <> makeDevDbUniqueId waspProjectDir appName

maxDockerContainerNameLength :: Int
maxDockerContainerNameLength = 63
