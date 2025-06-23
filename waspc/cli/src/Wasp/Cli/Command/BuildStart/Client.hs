module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Data.Function ((&))
import StrongPath ((</>))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort)
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
    buildDir = Config.buildDir config
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

startClient :: BuildStartConfig -> ExceptJob
startClient config =
  runNodeCommandAsJob
    webAppDir
    "npm"
    [ "exec",
      "vite",
      "--",
      "preview", -- `preview` launches vite just as a webserver to the built files.
      "--port",
      show defaultClientPort,
      "--strictPort" -- This will make it fail if the port is already in use.
    ]
    J.WebApp
    & toExceptJob (("Serving the client failed with exit code: " <>) . show)
  where
    buildDir = Config.buildDir config
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir
