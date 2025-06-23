module Wasp.Cli.Command.BuildStart.Config
  ( BuildStartConfig (..),
    clientPort,
    clientUrl,
    dockerContainerName,
    dockerImageName,
    serverUrl,
  )
where

import Data.Char (toLower)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort, getDefaultDevClientUrl)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)

data BuildStartConfig = BuildStartConfig
  { appSpec :: AppSpec,
    projectDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir),
    buildDir :: SP.Path' SP.Abs (SP.Dir ProjectRootDir)
  }

-- NOTE(carlos): For now, creating these URLs and ports uses the default values
-- we've hardcoded in the generator. In the future, we might want to make these
-- configurable via the Wasp app spec or command line arguments.

clientUrl :: BuildStartConfig -> String
clientUrl config = getDefaultDevClientUrl $ appSpec config

clientPort :: BuildStartConfig -> Int
clientPort _ = defaultClientPort

serverUrl :: BuildStartConfig -> String
serverUrl _ = defaultDevServerUrl

dockerImageName :: BuildStartConfig -> String
dockerImageName config = appUniqueId config <> "-server"

dockerContainerName :: BuildStartConfig -> String
dockerContainerName config = appUniqueId config <> "-server-container"

appUniqueId :: BuildStartConfig -> String
appUniqueId config = map toLower $ makeAppUniqueId (projectDir config) (appName config)

appName :: BuildStartConfig -> String
appName = fst . ASV.getApp . appSpec
