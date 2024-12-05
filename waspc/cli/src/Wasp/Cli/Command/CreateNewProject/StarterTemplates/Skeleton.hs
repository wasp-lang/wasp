{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Skeleton
  ( readWaspProjectSkeletonFiles,
  )
where

import Data.Text (Text)
import StrongPath (File', Path, Rel, System, reldir, (</>))
import qualified Wasp.Data as Data
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.IO (listDirectoryDeep, readFileStrict)

readWaspProjectSkeletonFiles :: IO [(Path System (Rel WaspProjectDir) File', Text)]
readWaspProjectSkeletonFiles = do
  skeletonFilesDir <- (</> [reldir|Cli/templates/skeleton|]) <$> Data.getAbsDataDirPath
  skeletonFilePaths <- listDirectoryDeep skeletonFilesDir
  mapM (\path -> (path,) <$> readFileStrict (skeletonFilesDir </> path)) skeletonFilePaths
