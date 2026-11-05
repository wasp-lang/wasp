module Wasp.Project.Studio
  ( startStudio,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.WaspNpmPackage (WaspNpmPackageScript (WaspStudioScript), runWaspNpmPackageScript)
import Wasp.Util (checkExitSuccess)

startStudio ::
  Path' Abs (Dir WaspProjectDir) ->
  -- | Path to the data JSON file.
  FilePath ->
  IO (Either String ())
startStudio waspProjectDir pathToDataFile =
  checkExitSuccess "Studio command"
    <$> runWaspNpmPackageScript waspProjectDir WaspStudioScript ["--data-file", pathToDataFile]
