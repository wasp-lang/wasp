module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Data.Function ((&))
import StrongPath ((</>))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig, clientPortAndUrl)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)

buildClient :: BuildStartConfig -> ExceptJob
buildClient config =
  runNodeCommandAsJobWithExtraEnv
    [("REACT_APP_API_URL", serverUrl)]
    webAppDir
    "npm"
    ["run", "build"]
    J.WebApp
    & toExceptJob (("Building the client failed with exit code: " <>) . show)
  where
    serverUrl = Config.serverUrl config
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir
    buildDir = Config.buildDir config

startClient :: BuildStartConfig -> ExceptJob
startClient config =
  runNodeCommandAsJob
    webAppDir
    "npm"
    [ "run",
      "preview", -- `preview` launches a static file server for the built client.
      "--",
      "--port",
      port,
      "--strictPort" -- This will make it fail if the port is already in use.
    ]
    J.WebApp
    & toExceptJob (("Serving the client failed with exit code: " <>) . show)
  where
    port = show $ fst $ Config.clientPortAndUrl config

    buildDir = Config.buildDir config
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir
