module Wasp.ConfigFiles
  ( discoverConfigFiles,
    ConfigFile (..),
  )
where

import Data.Map (fromList)
import qualified Data.Map as Data
import Data.Maybe (catMaybes)
import StrongPath (Abs, Dir, File', Path', Rel, basename, fromRelFile, relfile, (</>))
import Wasp.Common (WaspProjectDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import qualified Wasp.Util.IO as Util.IO

data ConfigFile = ConfigFile
  { _pathInWaspDir :: Path' Abs File',
    _projectRootDirPath :: Path' (Rel ProjectRootDir) File'
  }

configFileMappings :: Data.Map String (Path' (Rel ProjectRootDir) File')
configFileMappings =
  fromList
    [ ("tailwind.config.js", webAppRootDirInProjectRootDir </> [relfile|tailwind.config.js|]),
      ("postcss.config.js", webAppRootDirInProjectRootDir </> [relfile|postcss.config.js|])
    ]

-- | Discovers config files in the wasp project dir.
-- NOTE: In the future, we could allow devs to configure what files we look for and where we copy them to.
discoverConfigFiles :: Path' Abs (Dir WaspProjectDir) -> IO [ConfigFile]
discoverConfigFiles waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  let mappedConfigs = catMaybes $ map (fileToMaybeMapping waspDir) files
  return $ map (\(pathInWaspDir, projectRootDirPath) -> ConfigFile {_pathInWaspDir = pathInWaspDir, _projectRootDirPath = projectRootDirPath}) mappedConfigs

fileToMaybeMapping ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  Maybe (Path' Abs File', Path' (Rel ProjectRootDir) File')
fileToMaybeMapping waspDir file = do
  let filename = fromRelFile (basename file)
  case Data.lookup filename configFileMappings of
    Nothing -> Nothing
    Just projectRootDirPath -> do
      let pathInWaspDir = waspDir </> file
      Just (pathInWaspDir, projectRootDirPath)
