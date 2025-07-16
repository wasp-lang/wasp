module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig,
    buildDir,
    clientEnvironmentFiles,
    clientEnvironmentVariables,
    clientPortAndUrl,
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
    projectDir,
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
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort, getDefaultDevClientUrl)
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, makeAppUniqueId)

data BuildStartConfig = BuildStartConfig
  { _appName :: String,
    clientPortAndUrl :: (Int, String),
    serverEnvironmentVariables :: [String],
    serverEnvironmentFiles :: [FilePath],
    clientEnvironmentVariables :: [String],
    clientEnvironmentFiles :: [FilePath],
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> BuildStartArgs -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> BuildStartConfig
makeBuildStartConfig appSpec args =
  BuildStartConfig
    appName
    (clientPort, clientUrl)
    (Args.serverEnvironmentVariables args)
    (Args.serverEnvironmentFiles args)
    (Args.clientEnvironmentVariables args)
    (Args.clientEnvironmentFiles args)
  where
    (appName, _) = ASV.getApp appSpec

    -- This assumes that `getDefaultDevClientUrl` uses `defaultClientPort` internally.
    -- If that changes, we also need to change this.
    clientPort = defaultClientPort
    clientUrl = getDefaultDevClientUrl appSpec

buildDir :: BuildStartConfig -> SP.Path' SP.Abs (SP.Dir ProjectRootDir)
buildDir config =
  projectDir config </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

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

appUniqueId :: BuildStartConfig -> String
appUniqueId config =
  makeAppUniqueId (projectDir config) (_appName config)
