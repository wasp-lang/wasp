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
import Wasp.Cli.Util.EnvVarInputs (EnvVarsBySource, describeEnvVarSources, mergeEnvVars, resolveEnvVarInputs)
import Wasp.Cli.Util.Parser (getParserHelpMessage)
import Wasp.Env (EnvVarName)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Generator.WebAppGenerator.RunConfig (ClientRunConfig, makeClientRunConfig)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, makeAppUniqueId)
import qualified Wasp.Util.AppLocation as AL
import Wasp.Util.Terminal (styleCode)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    clientRunConfig :: ClientRunConfig,
    serverRunConfig :: ServerRunConfig,
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir),
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir' = do
  when (null args.clientEnvVarInputs && null args.serverEnvVarInputs) $ throwError noEnvVarsSpecifiedMsg

  serverEnvVars <- liftIO $ resolveEnvVarInputs projectDir' args.serverEnvVarInputs
  clientEnvVars <- liftIO $ resolveEnvVarInputs projectDir' args.clientEnvVarInputs

  serverRunConfig' <-
    mapOverriddenEnvVarsError serverEnvVars $
      makeServerRunConfig serverLocation (AL.url clientLocation) (mergeEnvVars serverEnvVars)
  clientRunConfig' <-
    mapOverriddenEnvVarsError clientEnvVars $
      makeClientRunConfig clientLocation (AL.url serverLocation) (mergeEnvVars clientEnvVars)

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        projectDir = projectDir',
        serverRunConfig = serverRunConfig',
        clientRunConfig = clientRunConfig'
      }
  where
    serverLocation = Server.defaultDevServerLocation
    clientLocation = WebApp.makeDefaultDevClientLocation appSpec

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

-- | The run config only tells us which env var names the user isn't allowed to
-- set, so we look them up in the inputs they came from to point the user at
-- the exact place they have to remove them from.
mapOverriddenEnvVarsError :: EnvVarsBySource -> Either [EnvVarName] a -> Command a
mapOverriddenEnvVarsError _ (Right runConfig) = return runConfig
mapOverriddenEnvVarsError envVarsBySource (Left overriddenNames) =
  throwError $
    CommandError "Overridden environment variables" $
      "The following env vars are set by Wasp and cannot be overridden by the user: "
        ++ describeEnvVarSources envVarsBySource overriddenNames

dockerImageName :: BuildStartConfig -> String
dockerImageName config =
  -- Lowercase because Docker image names require it.
  map toLower $ appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  -- Lowercase because Docker container names require it.
  map toLower $ appUniqueId config <> "-server-container"
