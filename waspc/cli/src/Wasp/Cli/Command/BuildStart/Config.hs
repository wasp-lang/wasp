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
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.Util.EnvVarInputs (resolveEnvVarInputs)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.Services (getWaspEnvVars)
import Wasp.Env (EnvVar)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import Wasp.Project.PerService (PerService)
import Wasp.Util.Terminal (styleCode)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    clientPort :: Int,
    envVars :: PerService [EnvVar],
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  when (all null args.envVarInputs) $ throwError noEnvVarsSpecifiedMsg

  let waspEnvVars = getWaspEnvVars appSpec

  envVars <- sequence $ resolveEnvVarInputs projectDir' <$> waspEnvVars <*> args.envVarInputs

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        projectDir = projectDir',
        clientPort = clientPort',
        envVars = envVars
      }
  where
    appUniqueId' = makeAppUniqueId projectDir' appName
    (appName, _) = ASV.getApp appSpec

    buildDir' = projectDir' </> generatedAppDirInWaspProjectDir

    -- NOTE(carlos): For now, this port (and the URLs inside 'getWaspEnvVars') uses
    -- the default values we've hardcoded in the generator. In the future, we might
    -- want to make these configurable via the Wasp app spec or command line arguments.

    -- This assumes that the client URL in 'getWaspEnvVars' uses `defaultClientPort`
    -- internally. If that changes, we also need to change this.
    clientPort' = defaultClientPort

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
