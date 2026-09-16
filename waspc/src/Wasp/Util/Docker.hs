module Wasp.Util.Docker
  ( DockerContainerName,
    DockerImageName,
    DockerVolumeMountPath,
    discoverHostPortForDockerContainersInternalPort,

    -- * Exported for testing only
    parseDockerPortOutput,
  )
where

import Control.Exception (SomeException, try)
import Data.List.Extra (takeWhileEnd)
import Network.Socket (PortNumber)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

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

-- | Parses the output of @docker port \<container\> \<port\>@ into the host port.
-- The output has a line per network interface, e.g. "0.0.0.0:5433\n[::]:5433\n".
parseDockerPortOutput :: String -> Maybe PortNumber
parseDockerPortOutput output = case lines output of
  (firstLine : _) -> readMaybe $ takeWhileEnd (/= ':') firstLine
  [] -> Nothing
