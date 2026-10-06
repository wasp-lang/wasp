module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (getEnvVars)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node
import qualified Wasp.Job.Process as JobProcess

buildClient :: BuildStartConfig -> Job.Job ()
buildClient config =
  JobProcess.run_
    =<< Node.command
      envVars
      projectDir
      "npx"
      ["vite", "build"]
  where
    envVars = getEnvVars config.clientRunConfig
    projectDir = config.projectDir

startClient :: BuildStartConfig -> Job.Job ()
startClient config =
  JobProcess.run_ . JobProcess.interactive
    =<< Node.command
      envVars
      projectDir
      "npx"
      [ "vite",
        "preview", -- `preview` launches a static file server for the built client.
        "--strictPort" -- This will make it fail if the port is already in use.
      ]
  where
    envVars = getEnvVars config.clientRunConfig
    projectDir = config.projectDir
