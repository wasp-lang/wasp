module Wasp.Cli.Command.Start.Db
  ( start,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.DevDb (startPostgresDevDb, throwIfCustomDbAlreadyInUse)
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import Wasp.Cli.Util.PortArgument (portParser)
import Wasp.Db.Postgres (defaultPostgresDockerImageSpec, defaultPostgresPort)
import qualified Wasp.Message as Msg
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
        (dbPort args)
        (dbImage args)
        (dbVolumeMountPath args)
  where
    noteSQLiteDoesntNeedStart =
      cliSendMessageC . Msg.Info $
        "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."

startDbArgsParser :: ArgsParser StartDbArgs
startDbArgsParser =
  ArgsParser "wasp start db" $
    StartDbArgs
      <$> Opt.optional (portParser "db-port" ("Port to run the dev database on (default: " ++ show defaultPostgresPort ++ ")"))
      <*> Opt.strOption
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
  { dbPort :: Maybe PortNumber,
    dbImage :: DockerImageName,
    dbVolumeMountPath :: DockerVolumeMountPath
  }
