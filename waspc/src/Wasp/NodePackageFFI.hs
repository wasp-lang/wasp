{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for setting up and running node processes from the
    -- @packages/@ directory.
    Package (..),
    getPackageProcessOptions,
  )
where

import Control.Monad.Extra (unlessM)
import StrongPath (Abs, Dir, File, Path', Rel, fromAbsDir, fromAbsFile, reldir, relfile, (</>))
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as P
import Wasp.Data (DataDir)
import qualified Wasp.Data as Data
import Wasp.Node.Version (getAndCheckNodeVersion)

data Package
  = DeployPackage
  | TsInspectPackage

data PackagesDir

data PackageDir

data PackageScript

packagesDirInDataDir :: Path' (Rel DataDir) (Dir PackagesDir)
packagesDirInDataDir = [reldir|packages|]

packageDirInPackagesDir :: Package -> Path' (Rel PackagesDir) (Dir PackageDir)
packageDirInPackagesDir DeployPackage = [reldir|deploy|]
packageDirInPackagesDir TsInspectPackage = [reldir|ts-inspect|]

scriptInPackageDir :: Path' (Rel PackageDir) (File PackageScript)
scriptInPackageDir = [relfile|dist/index.js|]

-- | Get a 'P.CreateProcess' for a particular package.
--
-- These packages are built during CI/locally via the @tools/install_packages_to_data_dir.sh@
-- script.
--
-- If the package does not have its dependencies installed yet (for example,
-- when the package is run for the first time after installing Wasp), we install
-- the dependencies.
getPackageProcessOptions :: Package -> [String] -> IO P.CreateProcess
getPackageProcessOptions package args = do
  getAndCheckNodeVersion >>= \case
    Right _ -> pure ()
    Left errorMsg -> do
      -- Exit if valid node version is not installed
      hPutStrLn stderr errorMsg
      exitFailure
  packageDir <- getPackageDir package
  let scriptFile = packageDir </> scriptInPackageDir
  ensurePackageDependenciesAreInstalled packageDir
  return $ packageCreateProcess packageDir "node" (fromAbsFile scriptFile : args)

getPackageDir :: Package -> IO (Path' Abs (Dir PackageDir))
getPackageDir package = do
  waspDataDir <- Data.getAbsDataDirPath
  let packageDir = waspDataDir </> packagesDirInDataDir </> packageDirInPackagesDir package
  return packageDir

-- | Runs @npm install@ if @node_modules@ does not exist in the package directory.
ensurePackageDependenciesAreInstalled :: Path' Abs (Dir PackageDir) -> IO ()
ensurePackageDependenciesAreInstalled packageDir =
  unlessM nodeModulesDirExists $ do
    let npmInstallCreateProcess = packageCreateProcess packageDir "npm" ["install"]
    (exitCode, _out, err) <- P.readCreateProcessWithExitCode npmInstallCreateProcess ""
    case exitCode of
      ExitFailure _ -> do
        -- Exit if node_modules fails to install
        hPutStrLn stderr $ "Failed to install NPM dependencies for package. Please report this issue: " ++ err
        exitFailure
      ExitSuccess -> pure ()
  where
    nodeModulesDirExists = doesDirectoryExist $ fromAbsDir nodeModulesDir
    nodeModulesDir = packageDir </> [reldir|node_modules|]

-- | Like 'P.proc', but sets up the cwd to the given package directory.
--
-- NOTE: do not export this function! users of this module should have to go
-- through 'getPackageProc', which makes sure node_modules are present.
packageCreateProcess ::
  Path' Abs (Dir PackageDir) ->
  String ->
  [String] ->
  P.CreateProcess
packageCreateProcess packageDir cmd args = (P.proc cmd args) {P.cwd = Just $ fromAbsDir packageDir}
