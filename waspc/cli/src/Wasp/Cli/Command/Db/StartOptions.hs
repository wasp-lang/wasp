module Wasp.Cli.Command.Db.StartOptions
  ( DbStartOptions (..),
    dbStartOptionsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Text.Printf (printf)
import Wasp.Cli.Util.PortArgument (portParser)
import Wasp.Db.Postgres (defaultPostgresPort)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)

data DbStartOptions = DbStartOptions
  { dbPort :: Maybe PortNumber,
    dbImage :: Maybe DockerImageName,
    dbVolumeMountPath :: Maybe DockerVolumeMountPath
  }
  deriving (Eq, Show)

dbStartOptionsParser :: Opt.Parser DbStartOptions
dbStartOptionsParser =
  DbStartOptions
    <$> Opt.optional (portParser "db-port" (printf "Port to run the dev database on (default: %s)" (show defaultPostgresPort)))
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
