module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig,
    appSpec,
    buildDir,
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
  { appSpec :: AppSpec,
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir),
    _args :: BuildStartArgs
  }

makeBuildStartConfig :: AppSpec -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> BuildStartArgs -> BuildStartConfig
makeBuildStartConfig = BuildStartConfig

buildDir :: BuildStartConfig -> SP.Path' SP.Abs (SP.Dir ProjectRootDir)
buildDir config =
  projectDir config </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

-- NOTE(carlos): For now, creating these URLs and ports uses the default values
-- we've hardcoded in the generator. In the future, we might want to make these
-- configurable via the Wasp app spec or command line arguments.

clientPortAndUrl :: BuildStartConfig -> (Int, String)
clientPortAndUrl config =
  -- This assumes that `getDefaultDevClientUrl` uses `defaultClientPort` internally.
  -- If that changes, we also need to change this.
  (defaultClientPort, getDefaultDevClientUrl $ appSpec config)

serverUrl :: BuildStartConfig -> String
serverUrl _ = defaultDevServerUrl

serverEnvironmentVariables :: BuildStartConfig -> [String]
serverEnvironmentVariables = Args.serverEnvironmentVariables . _args

serverEnvironmentFiles :: BuildStartConfig -> [FilePath]
serverEnvironmentFiles = Args.serverEnvironmentFiles . _args

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
  makeAppUniqueId (projectDir config) appName
  where
    (appName, _) = ASV.getApp $ appSpec config
