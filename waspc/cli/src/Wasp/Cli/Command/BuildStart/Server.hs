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
import qualified Wasp.Job as Job
import qualified Wasp.Job.Process as JobProcess
import Wasp.Process (InputMode (NoInput))

buildServer :: BuildStartConfig -> Job.Job ExitCode
buildServer config =
  JobProcess.run NoInput (proc "docker" ["build", "--tag", dockerImageName, dockerContextDir])
  where
    dockerContextDir = SP.fromAbsDir buildDir
    buildDir = config.buildDir
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> Job.Job ExitCode
startServer config =
  JobProcess.run NoInput $
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
