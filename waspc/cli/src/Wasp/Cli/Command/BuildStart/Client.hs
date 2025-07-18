module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Control.Monad.Except (MonadIO (liftIO))
import Data.Function ((&))
import StrongPath ((</>))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Cli.Util.EnvVarArgument (readEnvVarFile)
import Wasp.Env (EnvVar, nubEnvVars)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)

buildClient :: BuildStartConfig -> ExceptJob
buildClient config chan = do
  let envVarsFromArgs = Config.clientEnvironmentVariables config
  envVarsFromFiles <-
    liftIO $ mapM readEnvVarFile (Config.clientEnvironmentFiles config)
  let allEnvVars = envVarsFromArgs <> concat envVarsFromFiles

  buildClientWithUserDefinedEnvVars config allEnvVars chan

buildClientWithUserDefinedEnvVars :: BuildStartConfig -> [EnvVar] -> ExceptJob
buildClientWithUserDefinedEnvVars config userDefinedEnvVars =
  runNodeCommandAsJobWithExtraEnv
    allEnvVars
    webAppDir
    "npm"
    ["run", "build"]
    J.WebApp
    & toExceptJob (("Building the client failed with exit code: " <>) . show)
  where
    allEnvVars = nubEnvVars $ [("REACT_APP_API_URL", serverUrl)] <> userDefinedEnvVars
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
