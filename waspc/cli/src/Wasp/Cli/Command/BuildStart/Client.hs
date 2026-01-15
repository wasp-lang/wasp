module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Data.Function ((&))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)

buildClient :: BuildStartConfig -> ExceptJob
buildClient config =
  runNodeCommandAsJobWithExtraEnv
    envVars
    projectDir
    -- TODO: Missing `tsc --build` from the original `npm run build`.
    "npx"
    ["vite", "build"]
    J.WebApp
    & toExceptJob (("Building the client failed with exit code: " <>) . show)
  where
    envVars = Config.clientEnvVars config
    projectDir = Config.projectDir config

startClient :: BuildStartConfig -> ExceptJob
startClient config =
  runNodeCommandAsJob
    projectDir
    "npx"
    [ "vite",
      "preview", -- `preview` launches a static file server for the built client.
      "--port",
      port,
      "--strictPort" -- This will make it fail if the port is already in use.
    ]
    J.WebApp
    & toExceptJob (("Serving the client failed with exit code: " <>) . show)
  where
    port = show $ Config.clientPort config

    projectDir = Config.projectDir config
