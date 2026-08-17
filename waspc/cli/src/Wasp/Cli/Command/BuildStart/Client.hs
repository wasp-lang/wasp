module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
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
    envVars = Config.clientEnvVars config
    projectDir = Config.projectDir config

startClient :: BuildStartConfig -> Job.Job
startClient config =
  Node.makeJob
    projectDir
    "npx"
    [ "vite",
      "preview", -- `preview` launches a static file server for the built client.
      "--port",
      port,
      "--strictPort" -- This will make it fail if the port is already in use.
    ]
    Job.WebApp
  where
    port = show $ Config.clientPort config

    projectDir = Config.projectDir config
