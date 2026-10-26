module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import System.Exit (ExitCode)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import qualified Wasp.Job as Job

buildClient :: BuildStartConfig -> Job.Job ExitCode
buildClient config =
  Job.fromProc
    . Job.addEnvVarsOverride envVars
    . Job.setCwd config.projectDir
    $ Job.proc "vite" ["build"]
  where
    envVars = config.mergedClientEnvVars

startClient :: BuildStartConfig -> Job.Job ExitCode
startClient config =
  Job.fromProc
    . Job.addEnvVarsOverride envVars
    . Job.setCwd config.projectDir
    $ Job.proc
      "vite"
      [ "preview", -- `preview` launches a static file server for the built client.
        "--strictPort" -- This will make it fail if the port is already in use.
      ]
  where
    envVars = config.mergedClientEnvVars
