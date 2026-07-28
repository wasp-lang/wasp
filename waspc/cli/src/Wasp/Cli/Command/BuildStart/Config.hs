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
import Data.List (intercalate)
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.Util.AppSides (AppSides (..))
import qualified Wasp.Cli.Util.AppSides as AppSides
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import qualified Wasp.Cli.Util.PathArgument as PathArgument
import Wasp.Env (EnvVar, nubEnvVars, overrideEnvVars, parseDotEnvFile)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import Wasp.Util.Terminal (styleCode)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    ports :: AppSides Int,
    urls :: AppSides String,
    envVars :: AppSides [EnvVar],
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  userEnvVars <- traverse (liftIO . combineEnvVarsWithEnvFiles) args.envInputs
  when (all null userEnvVars) $ throwError noEnvVarsSpecifiedMsg

  let waspEnvVars =
        AppSides
          { client =
              [ (WebApp.serverUrlEnvVarName, urls.server)
              ],
            server =
              [ (Server.clientUrlEnvVarName, urls.client),
                (Server.serverUrlEnvVarName, urls.server)
              ]
          }

  envVars <- sequenceA $ liftA2 overrideEnvVarsCommand waspEnvVars userEnvVars

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        projectDir = projectDir',
        ports = ports,
        urls = urls,
        envVars = envVars
      }
  where
    appUniqueId' = makeAppUniqueId projectDir' appName
    (appName, _) = ASV.getApp appSpec

    buildDir' = projectDir' </> generatedAppDirInWaspProjectDir

    -- NOTE(carlos): For now, creating these URLs and ports below uses the default
    -- values we've hardcoded in the generator. In the future, we might want to make
    -- these configurable via the Wasp app spec or command line arguments.

    -- This assumes that `getDefaultDevClientUrl` uses `defaultClientPort` internally.
    -- If that changes, we also need to change this.
    ports = AppSides.defaultPorts
    urls = AppSides.defaultDevUrls appSpec

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

overrideEnvVarsCommand :: [EnvVar] -> [EnvVar] -> Command [EnvVar]
overrideEnvVarsCommand forced existing =
  case forced `overrideEnvVars` existing of
    Left duplicateNames ->
      throwError $
        CommandError "Duplicate environment variables" $
          ("The following environment variables will be overwritten by Wasp and should be removed: " <>) $
            intercalate ", " duplicateNames
    Right combined -> return combined

combineEnvVarsWithEnvFiles :: ([EnvVar], [FilePathArgument]) -> IO [EnvVar]
combineEnvVarsWithEnvFiles (inlineEnvVars, files) = do
  envVarsFromFiles <- mapM readEnvVarsFromFile files
  let allEnvVars = inlineEnvVars <> concat envVarsFromFiles
  return $ nubEnvVars allEnvVars

readEnvVarsFromFile :: FilePathArgument -> IO [EnvVar]
readEnvVarsFromFile pathArg = PathArgument.getFilePath pathArg >>= parseDotEnvFile
