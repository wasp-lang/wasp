module Wasp.ConfigFiles
  ( discoverConfigFiles,
    ConfigFile (..),
  )
where

import Data.Map (fromList)
import qualified Data.Map as Data
import Data.Maybe (mapMaybe)
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import Wasp.Common (WaspProjectDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import qualified Wasp.Util.IO as Util.IO

data ConfigFile = ConfigFile
  { _pathInWaspDir :: Path' Abs File',
    _projectRootDirPath :: Path' (Rel ProjectRootDir) File'
  }
  deriving (Show, Eq)

configFileMappings :: Data.Map (Path' (Rel WaspProjectDir) File') (Path' (Rel ProjectRootDir) File')
configFileMappings =
  fromList
    [ ([relfile|tailwind.config.js|], webAppRootDirInProjectRootDir </> [relfile|tailwind.config.js|]),
      ([relfile|postcss.config.js|], webAppRootDirInProjectRootDir </> [relfile|postcss.config.js|])
    ]

-- | Discovers config files in the wasp project dir.
-- NOTE: In the future, we could allow devs to configure what files we look for and where we copy them to.
discoverConfigFiles :: Path' Abs (Dir WaspProjectDir) -> IO [ConfigFile]
discoverConfigFiles waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ mapMaybe fileToMaybeConfigFile files
  where
    fileToMaybeConfigFile :: Path' (Rel WaspProjectDir) File' -> Maybe ConfigFile
    fileToMaybeConfigFile file = do
      case Data.lookup file configFileMappings of
        Nothing -> Nothing
        Just projectRootDirPath ->
          Just $ ConfigFile {_pathInWaspDir = waspDir </> file, _projectRootDirPath = projectRootDirPath}
