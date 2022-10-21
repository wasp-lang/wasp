module Wasp.ConfigFile
  ( discoverConfigFiles,
    tailwindConfigFile,
    postcssConfigFile,
    ConfigFileRelocator (..),
  )
where

import Data.Map (fromList)
import qualified Data.Map as Data
import Data.Maybe (mapMaybe)
import StrongPath (Abs, Dir, File', Path', Rel, castRel, relfile, (</>))
import Wasp.Common (WaspProjectDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import qualified Wasp.Util.IO as Util.IO

-- | A type for establishing the mapping of where to copy config files from/to.
data ConfigFileRelocator = ConfigFileRelocator
  { _pathInWaspDir :: Path' Abs File',
    _projectRootDirPath :: Path' (Rel ProjectRootDir) File'
  }
  deriving (Show, Eq)

tailwindConfigFile :: Path' (Rel WaspProjectDir) File'
tailwindConfigFile = [relfile|tailwind.config.js|]

postcssConfigFile :: Path' (Rel WaspProjectDir) File'
postcssConfigFile = [relfile|postcss.config.js|]

asProjectRootDirConfigFile :: Path' (Rel WaspProjectDir) File' -> Path' (Rel ProjectRootDir) File'
asProjectRootDirConfigFile = (webAppRootDirInProjectRootDir </>) . castRel

-- | Discovers config files of interest in the wasp project dir.
discoverConfigFiles :: Path' Abs (Dir WaspProjectDir) -> IO [ConfigFileRelocator]
discoverConfigFiles waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ mapMaybe fileToMaybeConfigFileRelocator files
  where
    fileToMaybeConfigFileRelocator :: Path' (Rel WaspProjectDir) File' -> Maybe ConfigFileRelocator
    fileToMaybeConfigFileRelocator file = do
      projectRootDirPath <- Data.lookup file configFileRelocationMap
      return $ ConfigFileRelocator {_pathInWaspDir = waspDir </> file, _projectRootDirPath = projectRootDirPath}

-- | Establishes the mapping of what config files to copy and where from/to.
-- NOTE: In the future, we could allow devs to configure what files we look for and where we copy them.
configFileRelocationMap :: Data.Map (Path' (Rel WaspProjectDir) File') (Path' (Rel ProjectRootDir) File')
configFileRelocationMap =
  fromList
    [ (tailwindConfigFile, asProjectRootDirConfigFile tailwindConfigFile),
      (postcssConfigFile, asProjectRootDirConfigFile postcssConfigFile)
    ]
