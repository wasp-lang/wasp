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
import Data.List.NonEmpty (toList)
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
import Wasp.Env (EnvVar, nubEnvVars, parseDotEnvFile)
import qualified Wasp.Generator.AppComponentUrl as AppComponentUrl
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.RunConfig (HasEnvVars, addEnvVars)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig, makeWebAppRunConfig)
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
  userServerEnvVars <-
    liftIO $
      combineEnvVarsWithEnvFiles (Args.serverEnvironmentVariables args) (Args.serverEnvironmentFiles args)
  userClientEnvVars <-
    liftIO $
      combineEnvVarsWithEnvFiles (Args.clientEnvironmentVariables args) (Args.clientEnvironmentFiles args)
  when (null userClientEnvVars && null userServerEnvVars) $ throwError noEnvVarsSpecifiedMsg

  let serverLocation = Server.defaultDevServerLocation
      clientLocation = WebApp.makeDefaultDevClientUrl appSpec

  serverRunConfig' <-
    makeServerRunConfig serverLocation (AppComponentUrl.url clientLocation)
      `addEnvVarsC` userServerEnvVars
  clientRunConfig' <-
    makeWebAppRunConfig clientLocation (AppComponentUrl.url serverLocation)
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

addEnvVarsC :: (HasEnvVars a) => a -> [EnvVar] -> Command a
addEnvVarsC x incomingEnvVars =
  either
    throwDuplicateEnvVarsError
    return
    (addEnvVars x incomingEnvVars)
  where
    throwDuplicateEnvVarsError duplicateEnvVarNames =
      throwError $
        CommandError
          "Duplicate environment variables"
          ( "The following environment variables will be overwritten by Wasp and should be removed: "
              <> intercalate ", " (toList duplicateEnvVarNames)
              <> "."
          )
