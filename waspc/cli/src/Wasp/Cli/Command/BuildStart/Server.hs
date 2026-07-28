module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import qualified StrongPath as SP
import System.Process (proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import qualified Wasp.Cli.Command.BuildStart.Job as BuildStartJob
import qualified Wasp.Job as Job
import qualified Wasp.Job.Subprocess as Subprocess

buildServer :: BuildStartConfig -> BuildStartJob.JobExecution
buildServer config =
  BuildStartJob.run (("Building the server failed with exit code: " <>) . show) $
    Job.makeJob Job.Server $ do
      exitCode <- Subprocess.run (proc "docker" ["build", "--tag", dockerImageName, dockerContextDir])
      Job.requireExitSuccess exitCode
  where
    dockerContextDir = SP.fromAbsDir buildDir
    buildDir = Config.buildDir config
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> BuildStartJob.JobExecution
startServer config =
  BuildStartJob.run (("Running the server failed with exit code: " <>) . show) $
    Job.makeJob Job.Server $ do
      exitCode <-
        Subprocess.run $
          proc
            "docker"
            ( ["run", "--name", dockerContainerName, "--rm", "--network", "host"]
                <> envVarParams
                <> [dockerImageName]
            )
      Job.requireExitSuccess exitCode
  where
    envVarParams = toEnvVarParams $ Config.serverEnvVars config
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config

    toEnvVarParams list =
      list >>= \(name, value) -> ["--env", name <> "=" <> value]
