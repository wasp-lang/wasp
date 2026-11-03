module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import System.Exit (ExitCode)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import qualified Wasp.Generator.NodeTools as NodeTools
import qualified Wasp.Job as Job

buildClient :: BuildStartConfig -> Job.Job ExitCode
buildClient config = NodeTools.buildWebApp config.clientRunConfig config.projectDir

startClient :: BuildStartConfig -> Job.Job ExitCode
startClient config = NodeTools.previewWebApp config.clientRunConfig config.projectDir
