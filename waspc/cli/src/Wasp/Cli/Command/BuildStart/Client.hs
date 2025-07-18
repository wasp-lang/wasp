module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Data.Function ((&))
import StrongPath ((</>))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Cli.Util.EnvVarArgument (forceEnvVarsM)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)

buildClient :: BuildStartConfig -> ExceptJob
buildClient config chan = do
  let serverUrl = Config.serverUrl config
      webAppDir = buildDir </> WebApp.webAppRootDirInProjectRootDir
      buildDir = Config.buildDir config

  allEnvVars <-
    forceEnvVarsM
      ("The following environment variables will be overwritten by Wasp and should be removed: " <>)
      [(WebApp.serverUrlEnvVarName, serverUrl)]
      (Config.clientEnvVars config)

  toExceptJob
    (("Building the client failed with exit code: " <>) . show)
    ( runNodeCommandAsJobWithExtraEnv
        allEnvVars
        webAppDir
        "npm"
        ["run", "build"]
        J.WebApp
    )
    chan

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
    webAppDir = buildDir </> WebApp.webAppRootDirInProjectRootDir
