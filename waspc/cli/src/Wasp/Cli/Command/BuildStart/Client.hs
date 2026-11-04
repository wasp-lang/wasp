module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import System.Exit (ExitCode)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (addEnvVarsOverride, getEnvVars)
import qualified Wasp.Generator.NodeTools as NodeTools
import qualified Wasp.Job as Job

buildClient :: BuildStartConfig -> Job.Job ExitCode
buildClient config =
  Job.fromProc
    . addEnvVarsOverride envVars
    $ NodeTools.proc config.projectDir NodeTools.buildWebApp
  where
    envVars = getEnvVars config.clientRunConfig

startClient :: BuildStartConfig -> Job.Job ExitCode
startClient config =
  Job.fromProc
    . addEnvVarsOverride envVars
    $ NodeTools.proc config.projectDir NodeTools.previewWebApp
  where
    envVars = getEnvVars config.clientRunConfig
