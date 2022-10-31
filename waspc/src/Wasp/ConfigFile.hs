module Wasp.ConfigFile
  ( ConfigFileRelocationMap,
    discoverConfigFiles,
  )
where

import qualified Data.Map as Data
import Data.Maybe (mapMaybe)
import StrongPath (Abs, Dir, File', Path', Rel, (</>))
import Wasp.AppSpec.ConfigFile (ConfigFileRelocator (..))
import Wasp.Common (WaspProjectDir)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Util.IO as Util.IO

type ConfigFileRelocationMap = Data.Map (Path' (Rel WaspProjectDir) File') (Path' (Rel ProjectRootDir) File')

-- | Discovers config files of interest in the wasp project dir.
discoverConfigFiles :: Path' Abs (Dir WaspProjectDir) -> ConfigFileRelocationMap -> IO [ConfigFileRelocator]
discoverConfigFiles waspDir configFileRelocationMap = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ mapMaybe fileToMaybeConfigFileRelocator files
  where
    fileToMaybeConfigFileRelocator :: Path' (Rel WaspProjectDir) File' -> Maybe ConfigFileRelocator
    fileToMaybeConfigFileRelocator file = do
      projectRootDirPath <- Data.lookup file configFileRelocationMap
      return $
        ConfigFileRelocator
          { _pathInWaspDir = waspDir </> file,
            _pathInProjectRootDir = projectRootDirPath
          }
