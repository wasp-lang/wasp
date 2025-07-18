module Wasp.Cli.Command.BuildStart.Client
  ( buildClient,
    startClient,
  )
where

import Control.Monad.Except (ExceptT, MonadIO (liftIO), liftEither)
import Data.Function ((&))
import StrongPath ((</>))
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Env (EnvVar, envVarFromString, nubEnvVars, parseDotEnvFilePath)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv)

buildClient :: BuildStartConfig -> ExceptJob
buildClient config chan = do
  envVars <- allClientEnvironmentVariables config
  buildClientWithExtraEnvVars config envVars chan

buildClientWithExtraEnvVars :: BuildStartConfig -> [EnvVar] -> ExceptJob
buildClientWithExtraEnvVars config userDefinedEnvVars =
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

allClientEnvironmentVariables :: BuildStartConfig -> ExceptT String IO [EnvVar]
allClientEnvironmentVariables config = do
  let envVarsFromStrings = Config.clientEnvironmentVariables config

  envVarsFromFiles <-
    liftIO $
      mapM parseDotEnvFilePath $ Config.clientEnvironmentFiles config

  return $ nubEnvVars (envVarsFromStrings <> concat envVarsFromFiles)
