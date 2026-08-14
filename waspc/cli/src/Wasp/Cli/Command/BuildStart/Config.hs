module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig (..),
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (toLower)
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.Util.EnvVarSource (EnvVarSource, addEnvVarsC, resolveEnvVarArguments, resolveEnvVarFile)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import Wasp.Env (EnvVar)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig, makeWebAppRunConfig)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import qualified Wasp.Util.AppLocation as AL
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
  when noEnvVarsSourcesSpecified $ throwError noEnvVarsSourcesSpecifiedMsg

  userServerEnvVars <- liftIO $ resolveEnvVarSources args.serverEnvVarSources
  userClientEnvVars <- liftIO $ resolveEnvVarSources args.clientEnvVarSources

  let serverLocation = Server.makeDevServerLocation args.serverPort
      clientLocation = WebApp.makeDevClientLocation appSpec args.clientPort

  serverRunConfig' <-
    makeServerRunConfig serverLocation (AL.url clientLocation)
      `addEnvVarsC` userServerEnvVars
  clientRunConfig' <-
    makeWebAppRunConfig clientLocation (AL.url serverLocation)
      `addEnvVarsC` userClientEnvVars

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

resolveEnvVarSources :: ([EnvVar], [FilePathArgument]) -> IO [EnvVarSource]
resolveEnvVarSources (argEnvVarSource, fileEnvVarSources) =
  concat
    <$> sequence
      [ return [resolveEnvVarArguments argEnvVarSource],
        mapM resolveEnvVarFile fileEnvVarSources
      ]

dockerImageName :: BuildStartConfig -> String
dockerImageName config =
  -- Lowercase because Docker image names require it.
  map toLower $ appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  -- Lowercase because Docker container names require it.
  map toLower $ appUniqueId config <> "-server-container"
