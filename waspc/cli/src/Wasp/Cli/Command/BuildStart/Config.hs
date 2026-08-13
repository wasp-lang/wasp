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
import Wasp.Cli.Util.EnvVarSource (EnvVarSource, overrideEnvVarsC, resolveEnvVarArguments, resolveEnvVarFile)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import qualified Wasp.Cli.Util.PortArgument as PortArgument
import Wasp.Env (EnvVar)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.ServerGenerator.RunConfig as Server.RC
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Generator.WebAppGenerator.RunConfig (ClientRunConfig)
import qualified Wasp.Generator.WebAppGenerator.RunConfig as WebApp.RC
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import qualified Wasp.Util.AppLocation as AL
import Wasp.Util.Terminal (styleCode)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    clientLocation :: AL.AppLocation,
    serverLocation :: AL.AppLocation,
    clientRunConfig :: ClientRunConfig,
    serverRunConfig :: Server.RC.ServerRunConfig,
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  when noEnvVarsSourcesSpecified $ throwError noEnvVarsSourcesSpecifiedMsg

  serverEnvVars <- liftIO $ resolveEnvVarSources args.serverEnvVarSources
  clientEnvVars <- liftIO $ resolveEnvVarSources args.clientEnvVarSources

  (clientPort, serverPort) <- PortArgument.resolveAppPorts args.clientPort args.serverPort

  let serverLocation' = Server.makeDevServerLocation serverPort
      clientLocation' = WebApp.makeDevClientLocation appSpec clientPort

      defaultServerRunConfig = Server.RC.makeServerRunConfig serverLocation' (AL.url clientLocation')
      defaultClientRunConfig = WebApp.RC.makeClientRunConfig clientLocation' (AL.url serverLocation')

  fullServerEnvVars <- overrideEnvVarsC defaultServerRunConfig.envVars serverEnvVars
  fullClientEnvVars <- overrideEnvVarsC defaultClientRunConfig.envVars clientEnvVars

  let serverRunConfig' = defaultServerRunConfig {Server.RC.envVars = fullServerEnvVars}
      clientRunConfig' = defaultClientRunConfig {WebApp.RC.envVars = fullClientEnvVars}

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        projectDir = projectDir',
        clientLocation = clientLocation',
        serverLocation = serverLocation',
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
