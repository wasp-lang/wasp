module Wasp.Cli.DevDb.ArgumentsParser
  ( DevDbOptions (..),
    devDbOptionsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (portParser)
import Wasp.Db.Postgres (defaultPostgresDockerImageSpec, defaultPostgresPort)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)

data DevDbOptions = DevDbOptions
  { dbPort :: Maybe PortNumber,
    dbImage :: DockerImageName,
    dbVolumeMountPath :: DockerVolumeMountPath
  }
  deriving (Eq, Show)

devDbOptionsParser :: Opt.Parser DevDbOptions
devDbOptionsParser =
  DevDbOptions
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
