module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig (..),
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (toLower)
import StrongPath ((</>))
import qualified StrongPath as SP
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.AppComponentUrls (defaultDevServerUrl, makeDefaultDevClientUrl)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.EnvVarWithCtx (addEnvVarsUniqueC)
import qualified Wasp.Cli.EnvVarWithCtx as EnvVarWithCtx
import Wasp.Cli.RunConfigs (makeRunConfigs)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import Wasp.Util.Terminal (styleCode)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    clientRunConfig :: WebAppRunConfig,
    serverRunConfig :: ServerRunConfig,
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  -- This is just a sanity check for the most common mistake, calling `wasp
  -- build start` without any env vars at all. We don't need to make an
  -- exhaustive check here as it's the generated apps' job to ensure they have
  -- the env vars they need.
  when (all null [args.clientEnvVars, args.serverEnvVars]) $
    throwError noEnvVarsSourcesSpecifiedMsg

  userServerEnvVars <- liftIO $ concatMapM EnvVarWithCtx.readEnvVarArgument args.serverEnvVars
  userClientEnvVars <- liftIO $ concatMapM EnvVarWithCtx.readEnvVarArgument args.clientEnvVars

  let clientUrl = (makeDefaultDevClientUrl appSpec) {AppComponentUrl.port = args.clientPort}
      serverUrl = defaultDevServerUrl {AppComponentUrl.port = args.serverPort}

      (baseClientRunConfig, baseServerRunConfig) = makeRunConfigs (clientUrl, serverUrl)

  clientRunConfig' <- baseClientRunConfig `addEnvVarsUniqueC` userClientEnvVars
  serverRunConfig' <- baseServerRunConfig `addEnvVarsUniqueC` userServerEnvVars

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        projectDir = projectDir',
        serverRunConfig = serverRunConfig',
        clientRunConfig = clientRunConfig'
      }
  where
    appUniqueId' = makeAppUniqueId projectDir' appName
    (appName, _) = ASV.getApp appSpec

    buildDir' = projectDir' </> generatedAppDirInWaspProjectDir

    noEnvVarsSourcesSpecifiedMsg =
      CommandError
        "No env vars specified"
        $ "You called "
          ++ styleCode "wasp build start"
          ++ " without specifying any environment variables for the started apps (client and server). This is likely a mistake, as all apps require some env vars: https://wasp.sh/docs/project/env-vars.\n\n"
          ++ "To faithfully simulate the production environment, "
          ++ styleCode "wasp build start"
          ++ " won't automatically read your "
          ++ styleCode ".env"
          ++ " files unless you explicitly tell it. "
          ++ getParserHelpMessage buildStartArgsParser

dockerImageName :: BuildStartConfig -> String
dockerImageName config =
  -- Lowercase because Docker image names require it.
  map toLower $ appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  -- Lowercase because Docker container names require it.
  map toLower $ appUniqueId config <> "-server-container"
