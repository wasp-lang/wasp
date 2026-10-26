module Wasp.Util.Docker
  ( DockerContainerName,
    DockerImageName,
    DockerVolumeMountPath,
    discoverHostPortForDockerContainersInternalPort,
    checkDockerDaemonConnection,

    -- * Exported for testing only
    parseDockerPortOutput,
  )
where

import Control.Exception (IOException, SomeException, try)
import Data.List.Extra (takeWhileEnd)
import Network.Socket (PortNumber)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import Wasp.Util (trim)

type DockerContainerName = String

type DockerImageName = String

type DockerVolumeMountPath = String

discoverHostPortForDockerContainersInternalPort :: DockerContainerName -> PortNumber -> IO (Maybe PortNumber)
discoverHostPortForDockerContainersInternalPort containerName containerPort = do
  result <-
    try $ readProcessWithExitCode "docker" ["port", containerName, show containerPort] ""
  return $ case result of
    Right (ExitSuccess, stdout, _stderr) -> parseDockerPortOutput stdout
    Right (ExitFailure _, _stdout, _stderr) -> Nothing
    Left (_ :: SomeException) -> Nothing

-- | Checks that the Docker daemon is running and that we can connect to it.
-- If we can't, returns the error that Docker reported.
checkDockerDaemonConnection :: IO (Either String ())
checkDockerDaemonConnection = do
  result <- try $ readProcessWithExitCode "docker" ["info"] ""
  return $ case result of
    Right (ExitSuccess, _stdout, _stderr) -> Right ()
    Right (ExitFailure _, _stdout, stderr) -> Left $ trim stderr
    Left (e :: IOException) -> Left $ show e

-- | Parses the output of @docker port \<container\> \<port\>@ into the host port.
-- The output has a line per network interface, e.g. "0.0.0.0:5433\n[::]:5433\n".
parseDockerPortOutput :: String -> Maybe PortNumber
parseDockerPortOutput output = case lines output of
  (firstLine : _) -> readMaybe $ takeWhileEnd (/= ':') firstLine
  [] -> Nothing
