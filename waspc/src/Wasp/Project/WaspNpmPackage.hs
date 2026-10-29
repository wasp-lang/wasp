module Wasp.Project.WaspNpmPackage
  ( -- * The Wasp npm package

    -- Wasp projects depend on Wasp through the @wasp/wasp npm package. It pins
    -- the Wasp CLI (@wasp/cli) and the Wasp Spec (@wasp/spec) to one version
    -- and ships the entry point the CLI uses to run the Spec analyzer.
    waspNpmPackageName,
    waspCliNpmPackageName,
    waspSpecNpmPackageName,
    waspSpecAnalyzerScriptInWaspProjectDir,
    tryGettingInstalledWaspNpmPackageVersion,

    -- * Development builds of the package
    DevPackages (..),
    getDevPackages,
    getWaspNpmPackageSpecifier,
    getDevPackageOverrides,
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, File', Path', Rel, castFile, fromAbsFile, parseAbsDir, parseRelFile, reldir, relfile, (</>))
import System.Environment (lookupEnv)
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil
import Wasp.Version (waspVersion)

waspNpmPackageName :: PJ.PackageName
waspNpmPackageName = "@wasp/wasp"

waspCliNpmPackageName :: PJ.PackageName
waspCliNpmPackageName = "@wasp/cli"

waspSpecNpmPackageName :: PJ.PackageName
waspSpecNpmPackageName = "@wasp/spec"

data WaspNpmPackageDir

-- | @wasp/wasp is a direct dependency of the project, so it is always at the
-- top level of node_modules, no matter which package manager laid it out.
waspNpmPackageDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir WaspNpmPackageDir)
waspNpmPackageDirInWaspProjectDir = nodeModulesDirInWaspProjectDir </> [reldir|@wasp/wasp|]

-- | The script that runs the Wasp Spec analyzer of the project's @wasp/spec.
-- We run it with `node` by path. From inside @wasp/wasp, Node resolves
-- @wasp/spec next to it under any package manager.
waspSpecAnalyzerScriptInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
waspSpecAnalyzerScriptInWaspProjectDir = waspNpmPackageDirInWaspProjectDir </> [relfile|run-spec.js|]

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

-- | While developing Wasp, the packages are not published yet, so projects
-- depend on tarballs packed from this repo instead (see
-- @waspc/tools/dev-packages/build.ts@). The CLI learns where they are from
-- the 'devPackagesDirEnvVarName' env var, which @waspc/tools/wasp-cli-dev@ sets.
--
-- TODO: Remove once the repo's Wasp apps join the pnpm workspace and no
-- longer depend on dev tarballs.
data DevPackages = DevPackages
  { waspTarball :: Path' Abs File',
    cliTarball :: Path' Abs File',
    specTarball :: Path' Abs File'
  }

devPackagesDirEnvVarName :: String
devPackagesDirEnvVarName = "WASP_DEV_PACKAGES_DIR"

getDevPackages :: IO (Maybe DevPackages)
getDevPackages =
  lookupEnv devPackagesDirEnvVarName >>= \case
    Nothing -> return Nothing
    Just devPackagesDirPath -> do
      devPackagesDir <- parseAbsDir devPackagesDirPath
      let devPackages =
            DevPackages
              { waspTarball = devPackagesDir </> tarballFile "wasp-wasp",
                cliTarball = devPackagesDir </> tarballFile "wasp-cli",
                specTarball = devPackagesDir </> tarballFile "wasp-spec"
              }
      forM_ [waspTarball, cliTarball, specTarball] $ \getTarball -> do
        let tarball = getTarball devPackages
        exists <- IOUtil.doesFileExist tarball
        unless exists
          $ ioError
          $ userError
          $ devPackagesDirEnvVarName
            ++ " is set, but "
            ++ fromAbsFile tarball
            ++ " does not exist. Run `./run build:dev-packages` in waspc/."
      return $ Just devPackages
  where
    -- Named the way `npm pack` / `pnpm pack` name tarballs.
    tarballFile :: String -> Path' (Rel d) File'
    tarballFile name = fromJust $ parseRelFile $ name ++ "-" ++ show waspVersion ++ ".tgz"

-- | What a new project's package.json should say for @wasp/wasp.
getWaspNpmPackageSpecifier :: Maybe DevPackages -> String
getWaspNpmPackageSpecifier = \case
  Nothing -> show waspVersion
  Just devPackages -> tarballSpecifier $ waspTarball devPackages

-- | npm `overrides` that make the dev @wasp/wasp pull in the dev @wasp/cli and
-- @wasp/spec tarballs instead of the (unpublished) versions it pins.
getDevPackageOverrides :: DevPackages -> [(PJ.PackageName, String)]
getDevPackageOverrides devPackages =
  [ (waspCliNpmPackageName, tarballSpecifier $ cliTarball devPackages),
    (waspSpecNpmPackageName, tarballSpecifier $ specTarball devPackages)
  ]

tarballSpecifier :: Path' Abs File' -> String
tarballSpecifier tarball = "file:" ++ fromAbsFile tarball
