module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig,
    buildDir,
    clientEnvVars,
    clientPort,
    clientUrl,
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
    serverEnvVars,
    serverUrl,
  )
where

import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Data.Char (toLower)
import Data.List (intercalate)
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs)
import qualified Wasp.Cli.Command.BuildStart.ArgumentsParser as Args
import Wasp.Cli.Util.EnvVarArgument (EnvVarFileArgument, readEnvVarFile)
import Wasp.Env (EnvVar, forceEnvVars, nubEnvVars)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort, getDefaultDevClientUrl)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, makeAppUniqueId)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    serverUrl :: String,
    clientPort :: Int,
    clientUrl :: String,
    serverEnvVars :: [EnvVar],
    clientEnvVars :: [EnvVar],
    buildDir :: SP.Path' SP.Abs (SP.Dir ProjectRootDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir = do
  serverEnvVars' <-
    readAndForceEnvVars
      [ (Server.clientUrlEnvVarName, clientUrl'),
        (Server.serverUrlEnvVarName, serverUrl')
      ]
      (Args.serverEnvironmentVariables args)
      (Args.serverEnvironmentFiles args)

  clientEnvVars' <-
    readAndForceEnvVars
      [ (WebApp.serverUrlEnvVarName, serverUrl')
      ]
      (Args.clientEnvironmentVariables args)
      (Args.clientEnvironmentFiles args)

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        serverUrl = serverUrl',
        clientPort = clientPort',
        clientUrl = clientUrl',
        serverEnvVars = serverEnvVars',
        clientEnvVars = clientEnvVars'
      }
  where
    appUniqueId' = makeAppUniqueId projectDir appName
    (appName, _) = ASV.getApp appSpec

    buildDir' = projectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

    -- NOTE(carlos): For now, creating these URLs and ports below uses the default
    -- values we've hardcoded in the generator. In the future, we might want to make
    -- these configurable via the Wasp app spec or command line arguments.

    -- This assumes that `getDefaultDevClientUrl` uses `defaultClientPort` internally.
    -- If that changes, we also need to change this.
    clientPort' = defaultClientPort
    clientUrl' = getDefaultDevClientUrl appSpec

    serverUrl' = defaultDevServerUrl

dockerImageName :: BuildStartConfig -> String
dockerImageName config =
  map toLower $ -- Lowercase because Docker image names require it.
    appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  map toLower $ -- Lowercase because Docker container names require it.
    appUniqueId config <> "-server-container"

readAndForceEnvVars :: [EnvVar] -> [EnvVar] -> [EnvVarFileArgument] -> Command [EnvVar]
readAndForceEnvVars forced existing files = do
  readVars <- liftIO $ readEnvVars existing files
  forceEnvVarsCommand forced readVars

readEnvVars :: [EnvVar] -> [EnvVarFileArgument] -> IO [EnvVar]
readEnvVars pairs files = do
  pairsFromFiles <- mapM readEnvVarFile files
  let allEnvVars = pairs <> concat pairsFromFiles
  return $ nubEnvVars allEnvVars

forceEnvVarsCommand :: [EnvVar] -> [EnvVar] -> Command [EnvVar]
forceEnvVarsCommand forced existing =
  case forceEnvVars forced existing of
    Left duplicateNames ->
      throwError $
        CommandError "Duplicate environment variables" $
          ("The following environment variables will be overwritten by Wasp and should be removed: " <>) $
            intercalate ", " duplicateNames
    Right combined -> return combined
