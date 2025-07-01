module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig,
    appSpec,
    buildDir,
    clientPortAndUrl,
    dockerContainerName,
    dockerImageName,
    makeBuildStartConfig,
    projectDir,
    serverUrl,
  )
where

import Data.Char (toLower)
import StrongPath ((</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort, getDefaultDevClientUrl)
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, makeAppUniqueId)

data BuildStartConfig = BuildStartConfig
  { appSpec :: AppSpec,
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)
  }

makeBuildStartConfig :: AppSpec -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> BuildStartConfig
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

dockerImageName :: BuildStartConfig -> String
dockerImageName config = appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config = appUniqueId config <> "-server-container"

appUniqueId :: BuildStartConfig -> String
appUniqueId config =
  map toLower $ -- Lowercase because Docker image names require it.
    makeAppUniqueId (projectDir config) appName
  where
    (appName, _) = ASV.getApp $ appSpec config
