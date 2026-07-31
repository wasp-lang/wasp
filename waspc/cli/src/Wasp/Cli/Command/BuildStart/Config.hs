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
import Network.Socket (PortNumber)
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs (..), buildStartArgsParser)
import Wasp.Cli.Util.Apps (defaultAppPorts, getDevUrlMakers, getWaspEnvVars)
import Wasp.Cli.Util.EnvVars (EnvVarSource, throwIfWaspOwnedEnvVarsAreSet)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import qualified Wasp.Cli.Util.PathArgument as PathArgument
import Wasp.Env (EnvVar, nubEnvVars, parseDotEnvFile)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Project.Apps (Apps (..))
import qualified Wasp.Project.Apps as Apps
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import Wasp.Util.Terminal (styleCode)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    ports :: Apps PortNumber,
    urls :: Apps String,
    envVars :: Apps [EnvVar],
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  let ports = defaultAppPorts

  userEnvVarsPerSource <- liftIO $ sequenceA $ readEnvInputs <$> Apps.names <*> args.envInputs
  let userEnvVars = nubEnvVars . concatMap snd <$> userEnvVarsPerSource
  when (all null userEnvVars) $ throwError noEnvVarsSpecifiedMsg

  let urls = getDevUrlMakers appSpec <*> ports
      waspEnvVars = getWaspEnvVars appSpec ports

  throwIfWaspOwnedEnvVarsAreSet
    "wasp build start"
    (map fst <$> waspEnvVars)
    (map (fmap (map fst)) <$> userEnvVarsPerSource)
  let envVars = liftA2 (<>) waspEnvVars userEnvVars

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

-- | Reads an app's env var inputs, keeping track of where each var came from so
-- collisions with wasp-owned vars can be attributed to their source.
readEnvInputs :: String -> ([EnvVar], [FilePathArgument]) -> IO [(EnvVarSource, [EnvVar])]
readEnvInputs appName (inlineEnvVars, files) = do
  fileEnvVars <- mapM readEnvVarsFromFile files
  return $ ("the --" <> appName <> "-env option", inlineEnvVars) : fileEnvVars

readEnvVarsFromFile :: FilePathArgument -> IO (EnvVarSource, [EnvVar])
readEnvVarsFromFile pathArg = do
  envFile <- PathArgument.getFilePath pathArg
  envVars <- parseDotEnvFile envFile
  return (SP.fromAbsFile envFile, envVars)
