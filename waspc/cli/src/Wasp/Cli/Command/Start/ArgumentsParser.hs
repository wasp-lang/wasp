module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
    StartDbArgs (..),
    startDbArgsParser,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.HttpUrlArgument (httpUrlParser)
import Wasp.Cli.Util.PortArgument (portParser)
import Wasp.Db.Postgres (defaultPostgresPort)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)

data StartArgs = StartArgs
  { clientPort :: Maybe PortNumber,
    serverPort :: Maybe PortNumber,
    clientUrl :: Maybe URI,
    serverUrl :: Maybe URI,
    dbArgs :: StartDbArgs
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> Opt.optional (portParser "client-port" "Port to run the client on")
    <*> Opt.optional (portParser "server-port" "Port to run the server on")
    <*> Opt.optional (httpUrlParser "client-url" "URL at which the client is reachable")
    <*> Opt.optional (httpUrlParser "server-url" "URL at which the server is reachable")
    <*> startDbArgsParser

data StartDbArgs = StartDbArgs
  { dbPort :: Maybe PortNumber,
    dbImage :: Maybe DockerImageName,
    dbVolumeMountPath :: Maybe DockerVolumeMountPath
  }
  deriving (Eq, Show)

startDbArgsParser :: Opt.Parser StartDbArgs
startDbArgsParser =
  StartDbArgs
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
