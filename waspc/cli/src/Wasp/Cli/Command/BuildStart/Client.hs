module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Data.Function ((&))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..))
import Wasp.Env (envVars)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)

buildClient :: BuildStartConfig -> ExceptJob
buildClient config =
  runNodeCommandAsJobWithExtraEnv
    envVars'
    projectDir
    "npx"
    ["vite", "build"]
    J.WebApp
    & toExceptJob (("Building the client failed with exit code: " <>) . show)
  where
    envVars' = envVars config.clientRunConfig
    projectDir = config.projectDir

startClient :: BuildStartConfig -> ExceptJob
startClient config =
  runNodeCommandAsJob
    projectDir
    "npx"
    [ "vite",
      "preview", -- `preview` launches a static file server for the built client.
      "--port",
      clientPort,
      "--strictPort" -- This will make it fail if the port is already in use.
    ]
    J.WebApp
    & toExceptJob (("Serving the client failed with exit code: " <>) . show)
  where
    clientPort = show config.clientRunConfig.port
    projectDir = config.projectDir
