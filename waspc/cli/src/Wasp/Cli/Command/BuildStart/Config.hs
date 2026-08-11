module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig (..),
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Data.Char (toLower)
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.AppComponents (makeDevRunConfigs)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.Util.EnvVarInputs (resolveEnvVarInputs)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.Client as Client
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.Server as Server
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import Wasp.Util.Terminal (styleCode)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    client :: Client.ClientRunConfig,
    server :: Server.ServerRunConfig,
    clientEnvVars :: [EnvVar],
    serverEnvVars :: [EnvVar],
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  when (null args.clientEnvVarInputs && null args.serverEnvVarInputs) $ throwError noEnvVarsSpecifiedMsg

  clientEnvVars' <- resolveEnvVarInputs projectDir' (Client.devEnvVars client') args.clientEnvVarInputs
  serverEnvVars' <- resolveEnvVarInputs projectDir' (Server.devEnvVars server') args.serverEnvVarInputs

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        client = client',
        server = server',
        clientEnvVars = clientEnvVars',
        serverEnvVars = serverEnvVars',
        buildDir = buildDir',
        projectDir = projectDir'
      }
  where
    (client', server') = makeDevRunConfigs appSpec args.clientPort args.serverPort

    appUniqueId' = makeAppUniqueId projectDir' appName
    (appName, _) = ASV.getApp appSpec

    buildDir' = projectDir' </> generatedAppDirInWaspProjectDir

    noEnvVarsSpecifiedMsg =
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
  map toLower $
    appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  -- Lowercase because Docker container names require it.
  map toLower $
    appUniqueId config <> "-server-container"
