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
import Wasp.Cli.AppComponents (makeDevRunConfigs)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import qualified Wasp.Cli.Util.PathArgument as PathArgument
import Wasp.Env (EnvVar, nubEnvVars, overrideEnvVars, parseDotEnvFile)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.Server.RunConfig as Server
import qualified Wasp.Generator.WebApp.RunConfig as Client
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
  userClientEnvVars <-
    liftIO $ combineEnvVarsWithEnvFiles args.clientEnvironmentVariables args.clientEnvironmentFiles
  userServerEnvVars <-
    liftIO $ combineEnvVarsWithEnvFiles args.serverEnvironmentVariables args.serverEnvironmentFiles
  when (null userClientEnvVars && null userServerEnvVars) $ throwError noEnvVarsSpecifiedMsg

  clientEnvVars' <- overrideEnvVarsCommand (Client.devEnvVars client') userClientEnvVars
  serverEnvVars' <- overrideEnvVarsCommand (Server.devEnvVars server') userServerEnvVars

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
    -- NOTE(carlos): For now, the run configs use the default ports we've
    -- hardcoded in the generator. In the future, we might want to make these
    -- configurable via the Wasp app spec or command line arguments.
    (client', server') = makeDevRunConfigs appSpec

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

overrideEnvVarsCommand :: [EnvVar] -> [EnvVar] -> Command [EnvVar]
overrideEnvVarsCommand forced existing =
  case forced `overrideEnvVars` existing of
    Left duplicateNames ->
      throwError $
        CommandError "Duplicate environment variables" $
          ("The following environment variables will be overwritten by Wasp and should be removed: " <>) $
            intercalate ", " duplicateNames
    Right combined -> return combined

combineEnvVarsWithEnvFiles :: [EnvVar] -> [FilePathArgument] -> IO [EnvVar]
combineEnvVarsWithEnvFiles inlineEnvVars files = do
  envVarsFromFiles <- mapM readEnvVarsFromFile files
  let allEnvVars = inlineEnvVars <> concat envVarsFromFiles
  return $ nubEnvVars allEnvVars

readEnvVarsFromFile :: FilePathArgument -> IO [EnvVar]
readEnvVarsFromFile pathArg = PathArgument.getFilePath pathArg >>= parseDotEnvFile
