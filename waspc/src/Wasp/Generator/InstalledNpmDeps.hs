module Wasp.Generator.InstalledNpmDeps
  ( findNotInstalledNpmDeps,
  )
where

import Control.Monad (filterM)
import Control.Monad.Extra (anyM)
import StrongPath (fromAbsDir)
import qualified System.Directory as SD
import qualified System.FilePath as FP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.OptionalFeatureNpmDeps (getNpmDepsOfUsedOptionalFeatures)
import Wasp.Generator.SdkGenerator.Common (sdkPackageName)

-- | The packages that the generated code needs and that are missing from the
-- `node_modules` of the project and of its parent dirs. It looks for the Wasp
-- SDK and for the packages of the features the app uses.
--
-- It doesn't look for \@wasp/wasp and its required peer dependencies. The CLI
-- runs through \@wasp/wasp, so it's installed, and npm installs its required
-- peer dependencies with it.
findNotInstalledNpmDeps :: AppSpec -> IO [P.PackageName]
findNotInstalledNpmDeps spec = filterM (fmap not . isInstalled) expectedPackageNames
  where
    expectedPackageNames = sdkPackageName : map Npm.Dependency.name (getNpmDepsOfUsedOptionalFeatures spec)

    -- A package manager puts the project's direct dependencies at the top of
    -- the project's `node_modules`, or of a parent dir's when it hoists them,
    -- e.g. in an npm workspace.
    isInstalled :: P.PackageName -> IO Bool
    isInstalled packageName =
      anyM (\dir -> SD.doesDirectoryExist $ dir FP.</> "node_modules" FP.</> packageName)
        $ selfAndParentDirs
        $ fromAbsDir
        $ AS.waspProjectDir spec

-- | The dir, its parent, and so on, up to the root.
selfAndParentDirs :: FilePath -> [FilePath]
selfAndParentDirs dir
  | parentDir == normalizedDir = [normalizedDir]
  | otherwise = normalizedDir : selfAndParentDirs parentDir
  where
    normalizedDir = FP.dropTrailingPathSeparator dir
    parentDir = FP.takeDirectory normalizedDir
