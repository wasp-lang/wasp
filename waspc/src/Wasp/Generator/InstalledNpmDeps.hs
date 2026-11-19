module Wasp.Generator.InstalledNpmDeps
  ( findNotInstalledNpmDeps,
  )
where

import Control.Monad (filterM)
import StrongPath (fromAbsDir, (</>))
import qualified System.Directory as SD
import qualified System.FilePath as FP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.OptionalNpmDeps (getAllDependencies, getNpmDepsOfUsedFeatures)
import Wasp.Generator.SdkGenerator.Common (sdkPackageName)
import Wasp.Project.Common (nodeModulesDirInWaspProjectDir)

-- | The packages that the generated code needs and that are missing from the
-- project's `node_modules`. It looks for the Wasp SDK and for the packages of
-- the features the app uses.
--
-- It doesn't look for \@wasp/wasp and its required peer dependencies. The CLI
-- runs through \@wasp/wasp, so they are installed.
findNotInstalledNpmDeps :: AppSpec -> IO [P.PackageName]
findNotInstalledNpmDeps spec = filterM (fmap not . isInstalled) expectedPackageNames
  where
    expectedPackageNames = sdkPackageName : map Npm.Dependency.name usedFeaturesDeps

    usedFeaturesDeps = concatMap getAllDependencies $ getNpmDepsOfUsedFeatures spec

    -- A package manager puts the project's direct dependencies at the top of
    -- its `node_modules`.
    isInstalled :: P.PackageName -> IO Bool
    isInstalled packageName = SD.doesDirectoryExist $ fromAbsDir nodeModulesDir FP.</> packageName

    nodeModulesDir = AS.waspProjectDir spec </> nodeModulesDirInWaspProjectDir
