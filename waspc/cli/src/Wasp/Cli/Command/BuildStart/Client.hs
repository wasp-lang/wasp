module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import System.Exit (ExitCode)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (addEnvVarsOverride)
import qualified Wasp.Generator.NodeCommand as NodeCommand
import qualified Wasp.Generator.WebAppGenerator.NodeCommands as WebAppNodeCommands
import qualified Wasp.Job as Job

buildClient :: BuildStartConfig -> Job.Job ExitCode
buildClient config =
  Job.fromProc
    . addEnvVarsOverride envVars
    $ NodeCommand.proc config.projectDir WebAppNodeCommands.buildWebApp
  where
    envVars = config.mergedClientEnvVars

startClient :: BuildStartConfig -> Job.Job ExitCode
startClient config =
  Job.fromProc
    . addEnvVarsOverride envVars
    $ NodeCommand.proc config.projectDir WebAppNodeCommands.previewWebApp
  where
    envVars = config.mergedClientEnvVars
