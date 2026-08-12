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
import qualified Wasp.Cli.Command.BuildStart.ArgumentsParser as Args
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import qualified Wasp.Cli.Util.PathArgument as PathArgument
import Wasp.Env (EnvVar, nubEnvVars, overrideEnvVars, parseDotEnvFile)
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
    clientRunConfig :: ClientRunConfig,
    serverRunConfig :: Server.RC.ServerRunConfig,
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  userServerEnvVars <-
    liftIO $
      combineEnvVarsWithEnvFiles (Args.serverEnvironmentVariables args) (Args.serverEnvironmentFiles args)
  userClientEnvVars <-
    liftIO $
      combineEnvVarsWithEnvFiles (Args.clientEnvironmentVariables args) (Args.clientEnvironmentFiles args)
  when (null userClientEnvVars && null userServerEnvVars) $ throwError noEnvVarsSpecifiedMsg

  let serverLocation = Server.defaultDevServerLocation
      clientLocation = WebApp.makeDefaultDevClientLocation appSpec

      defaultServerRunConfig = Server.RC.makeServerRunConfig serverLocation (AL.url clientLocation)
      defaultClientRunConfig = WebApp.RC.makeClientRunConfig clientLocation (AL.url serverLocation)

  fullServerEnvVars <- overrideEnvVarsC defaultServerRunConfig.envVars userServerEnvVars
  fullClientEnvVars <- overrideEnvVarsC defaultClientRunConfig.envVars userClientEnvVars

  let serverRunConfig' = defaultServerRunConfig {Server.RC.envVars = fullServerEnvVars}
      clientRunConfig' = defaultClientRunConfig {WebApp.RC.envVars = fullClientEnvVars}

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
  map toLower $ appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  -- Lowercase because Docker container names require it.
  map toLower $ appUniqueId config <> "-server-container"

combineEnvVarsWithEnvFiles :: [EnvVar] -> [FilePathArgument] -> IO [EnvVar]
combineEnvVarsWithEnvFiles inlineEnvVars files = do
  envVarsFromFiles <- mapM readEnvVarsFromFile files
  let allEnvVars = inlineEnvVars <> concat envVarsFromFiles
  return $ nubEnvVars allEnvVars

readEnvVarsFromFile :: FilePathArgument -> IO [EnvVar]
readEnvVarsFromFile pathArg = PathArgument.getFilePath pathArg >>= parseDotEnvFile

overrideEnvVarsC :: [EnvVar] -> [EnvVar] -> Command [EnvVar]
overrideEnvVarsC existingEnvVars incomingEnvVars =
  either
    throwDuplicateEnvVarsError
    return
    (overrideEnvVars existingEnvVars incomingEnvVars)
  where
    throwDuplicateEnvVarsError duplicateEnvVarNames =
      throwError $
        CommandError
          "Duplicate environment variables"
          ( "The following environment variables are defined multiple times: "
              <> intercalate ", " duplicateEnvVarNames
              <> ". Please remove the duplicates."
          )
