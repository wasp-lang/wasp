module Wasp.Project.WaspNpmPackage
  ( -- * The Wasp npm package

    -- Wasp projects depend on Wasp through the @wasp/wasp npm package. It pins
    -- the Wasp CLI (@wasp/cli), the Wasp Spec (@wasp/spec), @wasp/deploy and
    -- @wasp/studio to one version, and ships the scripts the CLI runs them with.
    waspNpmPackageName,
    waspCliNpmPackageName,
    waspSpecNpmPackageName,
    waspDeployNpmPackageName,
    waspStudioNpmPackageName,
    waspSpecAnalyzerScriptInWaspProjectDir,
    waspDeployScriptInWaspProjectDir,
    waspStudioScriptInWaspProjectDir,
    tryGettingInstalledWaspNpmPackageVersion,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import StrongPath (Abs, Dir, File, File', Path', Rel, castFile, fromAbsFile, reldir, relfile, (</>))
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil

waspNpmPackageName :: PJ.PackageName
waspNpmPackageName = "@wasp/wasp"

waspCliNpmPackageName :: PJ.PackageName
waspCliNpmPackageName = "@wasp/cli"

waspSpecNpmPackageName :: PJ.PackageName
waspSpecNpmPackageName = "@wasp/spec"

waspDeployNpmPackageName :: PJ.PackageName
waspDeployNpmPackageName = "@wasp/deploy"

waspStudioNpmPackageName :: PJ.PackageName
waspStudioNpmPackageName = "@wasp/studio"

data WaspNpmPackageDir

-- | \@wasp/wasp is a direct dependency of the project, so it is always at the
-- top level of node_modules, no matter which package manager laid it out.
waspNpmPackageDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir WaspNpmPackageDir)
waspNpmPackageDirInWaspProjectDir = nodeModulesDirInWaspProjectDir </> [reldir|@wasp/wasp|]

-- | The script that runs the Wasp Spec analyzer of the project's \@wasp/spec.
-- We run it with `node` by path. From inside \@wasp/wasp, Node resolves
-- \@wasp/spec next to it under any package manager.
waspSpecAnalyzerScriptInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
waspSpecAnalyzerScriptInWaspProjectDir = waspNpmPackageDirInWaspProjectDir </> [relfile|run-spec.js|]

-- | The script that runs the project's \@wasp/deploy, by path like
-- 'waspSpecAnalyzerScriptInWaspProjectDir'.
waspDeployScriptInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
waspDeployScriptInWaspProjectDir = waspNpmPackageDirInWaspProjectDir </> [relfile|run-deploy.js|]

-- | The script that runs the project's \@wasp/studio, by path like
-- 'waspSpecAnalyzerScriptInWaspProjectDir'.
waspStudioScriptInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
waspStudioScriptInWaspProjectDir = waspNpmPackageDirInWaspProjectDir </> [relfile|run-studio.js|]

tryGettingInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Either String SV.Version)
tryGettingInstalledWaspNpmPackageVersion projectDir = runExceptT $ do
  unlessM (liftIO $ IOUtil.doesFileExist packageJsonPath)
    $ throwError
    $ "Couldn't find " ++ fromAbsFile packageJsonPath
  packageJson <- ExceptT $ liftIO $ PJ.parsePackageJsonFile packageJsonPath
  ExceptT $ return $ case PJ.version packageJson of
    Just versionString -> first show $ SV.parseVersion versionString
    Nothing -> Left $ fromAbsFile packageJsonPath ++ " has no `version` field"
  where
    packageJsonPath :: Path' Abs (File InstalledPackageJsonFile)
    packageJsonPath =
      castFile $ projectDir </> waspNpmPackageDirInWaspProjectDir </> [relfile|package.json|]

data InstalledPackageJsonFile

instance PJ.PackageJsonFile InstalledPackageJsonFile
