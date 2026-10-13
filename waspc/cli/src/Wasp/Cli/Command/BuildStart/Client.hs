module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import System.Exit (ExitCode)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (getEnvVars, setEnvVars)
import qualified Wasp.Job as Job

buildClient :: BuildStartConfig -> Job.Job ExitCode
buildClient config =
  Job.fromProc . setEnvVars envVars
    =<< Job.npmBinProc config.projectDir "vite" ["build"]
  where
    envVars = getEnvVars config.clientRunConfig

startClient :: BuildStartConfig -> Job.Job ExitCode
startClient config =
  Job.fromProc . setEnvVars envVars
    =<< Job.npmBinProc
      config.projectDir
      "vite"
      [ "preview", -- `preview` launches a static file server for the built client.
        "--strictPort" -- This will make it fail if the port is already in use.
      ]
  where
    envVars = getEnvVars config.clientRunConfig
