module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
    StartDbArgs (..),
    startDbArgsParser,
    defaultStartDbArgs,
    parseDatabaseOptions,
  )
where

import Data.List (isPrefixOf, stripPrefix)
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (portOption)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)

data StartArgs = StartArgs
  { clientPort :: Maybe PortNumber,
    serverPort :: Maybe PortNumber,
    dbArgs :: StartDbArgs
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> portOption "client-port" "Port to run the client on"
    <*> portOption "server-port" "Port to run the server on"
    <*> startDbArgsParser

data StartDbArgs = StartDbArgs
  { dbImage :: Maybe DockerImageName,
    dbVolumeMountPath :: Maybe DockerVolumeMountPath
  }
  deriving (Eq, Show)

defaultStartDbArgs :: StartDbArgs
defaultStartDbArgs = StartDbArgs Nothing Nothing

startDbArgsParser :: Opt.Parser StartDbArgs
startDbArgsParser =
  StartDbArgs
    <$> Opt.optional
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

parseDatabaseOptions :: [String] -> Either String (StartDbArgs, [String])
parseDatabaseOptions = go defaultStartDbArgs []
  where
    go args commandArgs [] = Right (args, reverse commandArgs)
    go args commandArgs ("--db-image" : image : rest)
      | "--" `isPrefixOf` image = Left "Provide an image after --db-image."
      | otherwise = go args {dbImage = Just image} commandArgs rest
    go args commandArgs ("--db-volume-mount-path" : path : rest)
      | "--" `isPrefixOf` path = Left "Provide a path after --db-volume-mount-path."
      | otherwise = go args {dbVolumeMountPath = Just path} commandArgs rest
    go args commandArgs ("--name" : name : rest) = go args (name : "--name" : commandArgs) rest
    go _ _ ["--db-image"] = Left "Provide an image after --db-image."
    go _ _ ["--db-volume-mount-path"] = Left "Provide a path after --db-volume-mount-path."
    go args commandArgs (argument : rest)
      | Just image <- stripPrefix "--db-image=" argument = go args {dbImage = Just image} commandArgs rest
      | Just path <- stripPrefix "--db-volume-mount-path=" argument = go args {dbVolumeMountPath = Just path} commandArgs rest
      | otherwise = go args (argument : commandArgs) rest
