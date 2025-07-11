module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import Data.Function ((&))
import qualified StrongPath as SP
import System.Process (proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Generator.ServerGenerator.Common (clientUrlFromServerEnvVarName, serverUrlFromServerEnvVarName)
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runProcessAsJob)

buildServer :: BuildStartConfig -> ExceptJob
buildServer config =
  runProcessAsJob
    (proc "docker" ["build", "--tag", dockerImageName, dockerContextDir])
    J.Server
    & toExceptJob (("Building the server failed with exit code: " <>) . show)
  where
    buildDir = Config.buildDir config
    dockerContextDir = SP.fromAbsDir buildDir
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> ExceptJob
startServer config =
  ( \chan -> do
      runProcessAsJob
        ( proc
            "docker"
            ( [ "run",
                "--name",
                dockerContainerName,
                "--rm",
                "--network",
                "host",
                "--env",
                clientUrlFromServerEnvVarName <> "=" <> clientUrl,
                "--env",
                serverUrlFromServerEnvVarName <> "=" <> serverUrl
              ]
                <> extraEnvFileParamsFromConfig
                <> extraEnvParamsFromConfig
                <> [dockerImageName]
            )
        )
        J.Server
        chan
  )
    & toExceptJob (("Running the server failed with exit code: " <>) . show)
  where
    (_, clientUrl) = Config.clientPortAndUrl config
    serverUrl = Config.serverUrl config
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config

    extraEnvParamsFromConfig =
      Config.serverEnvironmentVariables config
        >>= \envVar -> ["--env", envVar]

    extraEnvFileParamsFromConfig =
      Config.serverEnvironmentFiles config
        >>= \envFile -> ["--env-file", envFile]
