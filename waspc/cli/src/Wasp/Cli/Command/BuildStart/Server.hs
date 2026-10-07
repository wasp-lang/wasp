module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import qualified StrongPath as SP
import System.Exit (ExitCode)
import System.Process (proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Env (getEnvVars)
import qualified Wasp.Job.Fictional as Job

buildServer :: BuildStartConfig -> Job.Job e ExitCode
buildServer config =
  Job.fromProc $
    proc
      "docker"
      ["build", "--tag", dockerImageName, dockerContextDir]
  where
    dockerContextDir = SP.fromAbsDir buildDir
    buildDir = config.buildDir
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> Job.Job e ExitCode
startServer config =
  Job.fromProc $
    proc
      "docker"
      ( ["run", "--name", dockerContainerName, "--rm", "--network", "host"]
          <> envVarParams
          <> [dockerImageName]
      )
  where
    envVarParams = toEnvVarParams $ getEnvVars config.serverRunConfig
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config

    toEnvVarParams list =
      list >>= \(name, value) -> ["--env", name <> "=" <> value]
