module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (getEnvVars)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node

buildClient :: BuildStartConfig -> Job.Job
buildClient config =
  Node.makeJobWithExtraEnv
    envVars
    projectDir
    "npx"
    ["vite", "build"]
    Job.WebApp
  where
    envVars = getEnvVars config.clientRunConfig
    projectDir = config.projectDir

startClient :: BuildStartConfig -> Job.Job
startClient config =
  Node.makeJobWithExtraEnv
    envVars
    projectDir
    "npx"
    [ "vite",
      "preview", -- `preview` launches a static file server for the built client.
      "--strictPort" -- This will make it fail if the port is already in use.
    ]
    Job.WebApp
  where
    envVars = getEnvVars config.clientRunConfig
    projectDir = config.projectDir
