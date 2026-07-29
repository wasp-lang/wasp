module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import qualified StrongPath as SP
import System.Process (proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import qualified Wasp.Job as Job
import qualified Wasp.Job.Subprocess as Subprocess

buildServer :: BuildStartConfig -> Job.Job
buildServer config =
  Job.makeJob Job.Server $
    Subprocess.run (proc "docker" ["build", "--tag", dockerImageName, dockerContextDir])
  where
    dockerContextDir = SP.fromAbsDir buildDir
    buildDir = Config.buildDir config
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> Job.Job
startServer config =
  Job.makeJob Job.Server $
    Subprocess.run $
      proc
        "docker"
        ( ["run", "--name", dockerContainerName, "--rm", "--network", "host"]
            <> envVarParams
            <> [dockerImageName]
        )
  where
    envVarParams = toEnvVarParams $ Config.serverEnvVars config
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config

    toEnvVarParams list =
      list >>= \(name, value) -> ["--env", name <> "=" <> value]
