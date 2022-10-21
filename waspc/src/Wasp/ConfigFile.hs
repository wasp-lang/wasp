module Wasp.ConfigFile
  ( discoverConfigFiles,
    tailwindConfigFile,
    postcssConfigFile,
    ConfigFile (..),
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
data ConfigFile = ConfigFile
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

-- | Establishes the mapping of where to copy config files from/to.
-- NOTE: In the future, we could allow devs to configure what files we look for and where we copy them to.
configMapping :: Data.Map (Path' (Rel WaspProjectDir) File') (Path' (Rel ProjectRootDir) File')
configMapping =
  fromList
    [ (tailwindConfigFile, asProjectRootDirConfigFile tailwindConfigFile),
      (postcssConfigFile, asProjectRootDirConfigFile postcssConfigFile)
    ]

-- | Discovers config files in the wasp project dir.
discoverConfigFiles :: Path' Abs (Dir WaspProjectDir) -> IO [ConfigFile]
discoverConfigFiles waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ mapMaybe fileToMaybeConfigFile files
  where
    fileToMaybeConfigFile :: Path' (Rel WaspProjectDir) File' -> Maybe ConfigFile
    fileToMaybeConfigFile file = do
      case Data.lookup file configMapping of
        Nothing -> Nothing
        Just projectRootDirPath ->
          Just $ ConfigFile {_pathInWaspDir = waspDir </> file, _projectRootDirPath = projectRootDirPath}
