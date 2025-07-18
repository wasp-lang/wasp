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
import Wasp.Cli.Util.EnvVarArgument (forceEnvVarsM)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
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
    dockerContextDir = SP.fromAbsDir buildDir
    buildDir = Config.buildDir config
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> ExceptJob
startServer config chan = do
  let (_, clientUrl) = Config.clientPortAndUrl config
      serverUrl = Config.serverUrl config
      dockerContainerName = Config.dockerContainerName config
      dockerImageName = Config.dockerImageName config

  allEnvVars <-
    forceEnvVarsM
      ("The following environment variables will be overwritten by Wasp and should be removed: " <>)
      [ (Server.clientUrlEnvVarName, clientUrl),
        (Server.serverUrlEnvVarName, serverUrl)
      ]
      (Config.serverEnvVars config)

  toExceptJob
    (("Running the server failed with exit code: " <>) . show)
    ( runProcessAsJob
        ( proc
            "docker"
            ( ["run", "--name", dockerContainerName, "--rm", "--network", "host"]
                <> toEnvVarParams allEnvVars
                <> [dockerImageName]
            )
        )
        J.Server
    )
    chan

toEnvVarParams :: [EnvVar] -> [String]
toEnvVarParams list =
  list
    >>= \(name, value) -> ["--env", name <> "=" <> value]
