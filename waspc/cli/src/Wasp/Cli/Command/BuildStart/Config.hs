module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig,
    appUrl,
    buildDir,
    clientEnvVars,
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
    serverEnvVars,
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
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs, buildStartArgsParser)
import qualified Wasp.Cli.Command.BuildStart.ArgumentsParser as Args
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
    -- | Where the built app is served from: one container serves the whole app,
    -- its pages and its API alike.
    appUrl :: String,
    serverEnvVars :: [EnvVar],
    clientEnvVars :: [EnvVar],
    buildDir :: SP.Path' SP.Abs (SP.Dir GeneratedAppDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command BuildStartConfig
makeBuildStartConfig appSpec args projectDir = do
  userServerEnvVars <-
    liftIO $
      combineEnvVarsWithEnvFiles (Args.serverEnvironmentVariables args) (Args.serverEnvironmentFiles args)
  userClientEnvVars <-
    liftIO $
      combineEnvVarsWithEnvFiles (Args.clientEnvironmentVariables args) (Args.clientEnvironmentFiles args)
  when (null userClientEnvVars && null userServerEnvVars) $ throwError noEnvVarsSpecifiedMsg

  -- One server serves the app's pages and its API, so both of these are the
  -- app's own URL.
  let waspServerEnvVars =
        [ (Server.clientUrlEnvVarName, appUrl'),
          (Server.serverUrlEnvVarName, appUrl')
        ]
  serverEnvVars' <- overrideEnvVarsCommand waspServerEnvVars userServerEnvVars

  return $
    BuildStartConfig
      { appUniqueId = appUniqueId',
        buildDir = buildDir',
        appUrl = appUrl',
        serverEnvVars = serverEnvVars',
        -- Nothing to force here: the client finds the API on the app's own
        -- origin, which is where it already is.
        clientEnvVars = userClientEnvVars
      }
  where
    appUniqueId' = makeAppUniqueId projectDir appName
    (appName, _) = ASV.getApp appSpec

    buildDir' = projectDir </> generatedAppDirInWaspProjectDir

    -- NOTE(carlos): For now, creating this URL uses the default values we've
    -- hardcoded in the generator. In the future, we might want to make it
    -- configurable via the Wasp app spec or command line arguments.
    appUrl' = Server.defaultServerUrl <> SP.fromAbsDirP (WebApp.getBaseDir appSpec)

    noEnvVarsSpecifiedMsg =
      CommandError
        "No env vars specified"
        $ "You called "
          ++ styleCode "wasp build start"
          ++ " without specifying any environment variables for the started app. This is likely a mistake, as all apps require some env vars: https://wasp.sh/docs/project/env-vars.\n\n"
          ++ "To faithfully simulate the production environment, "
          ++ styleCode "wasp build start"
          ++ " won't automatically read your "
          ++ styleCode ".env"
          ++ " files unless you explicitly tell it. "
          ++ getParserHelpMessage buildStartArgsParser

dockerImageName :: BuildStartConfig -> String
dockerImageName config =
  -- Lowercase because Docker image names require it.
  map toLower $ appUniqueId config

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  -- Lowercase because Docker container names require it.
  map toLower $ appUniqueId config <> "-container"

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
