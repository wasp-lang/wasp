module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig,
    buildDir,
    clientEnvironmentFiles,
    clientEnvironmentVariables,
    clientPortAndUrl,
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
    serverEnvironmentFiles,
    serverEnvironmentVariables,
    serverUrl,
  )
where

import Data.Char (toLower)
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command.BuildStart.ArgumentsParser (BuildStartArgs)
import qualified Wasp.Cli.Command.BuildStart.ArgumentsParser as Args
import Wasp.Env (EnvVar)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort, getDefaultDevClientUrl)
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, makeAppUniqueId)

data BuildStartConfig = BuildStartConfig
  { appUniqueId :: String,
    clientPortAndUrl :: (Int, String),
    serverEnvironmentVariables :: [EnvVar],
    serverEnvironmentFiles :: [FilePath],
    clientEnvironmentVariables :: [EnvVar],
    clientEnvironmentFiles :: [FilePath],
    buildDir :: SP.Path' SP.Abs (SP.Dir ProjectRootDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> BuildStartConfig
makeBuildStartConfig appSpec args projectDir =
  BuildStartConfig
    { appUniqueId = appUniqueId',
      clientPortAndUrl = (clientPort, clientUrl),
      serverEnvironmentVariables = Args.serverEnvironmentVariables args,
      serverEnvironmentFiles = Args.serverEnvironmentFiles args,
      clientEnvironmentVariables = Args.clientEnvironmentVariables args,
      clientEnvironmentFiles = Args.clientEnvironmentFiles args,
      buildDir = buildDir'
    }
  where
    buildDir' = projectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

    appUniqueId' = makeAppUniqueId projectDir appName

    (appName, _) = ASV.getApp appSpec

    -- This assumes that `getDefaultDevClientUrl` uses `defaultClientPort` internally.
    -- If that changes, we also need to change this.
    clientPort = defaultClientPort
    clientUrl = getDefaultDevClientUrl appSpec

-- NOTE(carlos): For now, creating these URLs and ports uses the default values
-- we've hardcoded in the generator. In the future, we might want to make these
-- configurable via the Wasp app spec or command line arguments.

serverUrl :: BuildStartConfig -> String
serverUrl _ = defaultDevServerUrl

dockerImageName :: BuildStartConfig -> String
dockerImageName config =
  map toLower $ -- Lowercase because Docker image names require it.
    appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config =
  map toLower $ -- Lowercase because Docker container names require it.
    appUniqueId config <> "-server-container"
