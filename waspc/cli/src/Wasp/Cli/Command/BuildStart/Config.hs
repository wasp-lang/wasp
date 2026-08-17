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
import Wasp.AppComponentUrl (AppComponentUrl)
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.EnvVarCtx (EnvVarWithCtx, addEnvVarsUniqueC)
import qualified Wasp.Cli.EnvVarCtx as EnvVarCtx
import Wasp.Cli.RunConfigs (defaultDevServerUrl, makeDefaultDevClientUrl, makeDevDefaultRunConfigs)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import Wasp.Env (EnvVar)
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
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir),
    -- These are only needed for showing the apps' URLs in the CLI:
    clientUrl :: AppComponentUrl,
    serverUrl :: AppComponentUrl
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  when noEnvVarsSourcesSpecified $ throwError noEnvVarsSourcesSpecifiedMsg

  userClientEnvVars <- liftIO $ getEnvVarsWithCtx args.clientEnvVarSources
  userServerEnvVars <- liftIO $ getEnvVarsWithCtx args.serverEnvVarSources

  let clientUrl = (makeDefaultDevClientUrl appSpec) {AppComponentUrl.port = args.clientPort}
      serverUrl = defaultDevServerUrl {AppComponentUrl.port = args.serverPort}

      (defaultClientRunConfig, defaultServerRunConfig) = makeDevDefaultRunConfigs appSpec

  clientRunConfig' <- defaultClientRunConfig `addEnvVarsUniqueC` userClientEnvVars
  serverRunConfig' <- defaultServerRunConfig `addEnvVarsUniqueC` userServerEnvVars

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        projectDir = projectDir',
        clientUrl = clientUrl,
        serverUrl = serverUrl,
        serverRunConfig = serverRunConfig',
        clientRunConfig = clientRunConfig'
      }
  where
    appUniqueId' = makeAppUniqueId projectDir' appName
    (appName, _) = ASV.getApp appSpec

    buildDir' = projectDir' </> generatedAppDirInWaspProjectDir

    noEnvVarsSourcesSpecified =
      null (fst args.clientEnvVarSources)
        && null (snd args.clientEnvVarSources)
        && null (fst args.serverEnvVarSources)
        && null (snd args.serverEnvVarSources)

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

getEnvVarsWithCtx :: ([EnvVar], [FilePathArgument]) -> IO [EnvVarWithCtx]
getEnvVarsWithCtx (argEnvVarSource, fileEnvVarSources) =
  concat
    <$> sequence
      [ return $ EnvVarCtx.fromCliArguments <$> argEnvVarSource,
        concatMapM EnvVarCtx.fromFilePathArgument fileEnvVarSources
      ]

dockerImageName :: BuildStartConfig -> String
dockerImageName config =
  -- Lowercase because Docker image names require it.
  map toLower $ appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  -- Lowercase because Docker container names require it.
  map toLower $ appUniqueId config <> "-server-container"
