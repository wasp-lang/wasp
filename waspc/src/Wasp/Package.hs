{-# LANGUAGE DeriveAnyClass #-}

module Wasp.Package
  ( Package (..),
    getPackageProc,
  )
where

import Control.Monad.Extra (unlessM, void)
import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, reldir, (</>))
import System.Directory (doesDirectoryExist)
import qualified System.Process as P
import Wasp.Data (DataDir)
import qualified Wasp.Data as Data

data Package
  = DeployPackage
  | TsInspectPackage

data PackagesDir

data PackageDir

packagesDirInDataDir :: Path' (Rel DataDir) (Dir PackagesDir)
packagesDirInDataDir = [reldir|packages|]

packageDirInPackagesDir :: Package -> Path' (Rel PackagesDir) (Dir PackageDir)
packageDirInPackagesDir DeployPackage = [reldir|deploy|]
packageDirInPackagesDir TsInspectPackage = [reldir|ts-inspect|]

-- | Get a 'P.CreateProcess' for a particular package.
--
-- These packages are built during CI/locally via the @tools/install_packages_to_data_dir.sh@
-- script.
--
-- If the package does not have its dependencies installed yet (i.e. after they
-- just installed a Wasp version), we install the dependencies.
getPackageProc :: Package -> [String] -> IO P.CreateProcess
getPackageProc package args = do
  packageDir <- getPackageDir package
  ensurePackageDependenciesAreInstalled packageDir
  -- @--@ is present to make sure @args@ get sent to the process inside the
  -- npm start command instead of npm itself.
  return $ packageProc packageDir "npm" $ concat [["npm", "--"], args]

getPackageDir :: Package -> IO (Path' Abs (Dir PackageDir))
getPackageDir package = do
  waspDataDir <- Data.getAbsDataDirPath
  let packageDir = waspDataDir </> packagesDirInDataDir </> packageDirInPackagesDir package
  return packageDir

-- | Runs @npm install@ if @node_modules@ does not exist in the package directory.
ensurePackageDependenciesAreInstalled :: Path' Abs (Dir PackageDir) -> IO ()
ensurePackageDependenciesAreInstalled packageDir =
  unlessM nodeModulesDirExists $ do
    let npmInstallCreateProcess = packageProc packageDir "npm" ["install"]
    void $ P.readCreateProcess npmInstallCreateProcess ""
  where
    nodeModulesDirExists = doesDirectoryExist $ fromAbsDir nodeModulesDir
    nodeModulesDir = packageDir </> [reldir|node_modules|]

-- | Like 'P.proc', but sets up the cwd to the given package directory.
--
-- NOTE: do not export this function! users of this module should have to go
-- through 'getPackageProc', which makes sure node_modules are present.
packageProc ::
  Path' Abs (Dir PackageDir) ->
  String ->
  [String] ->
  P.CreateProcess
packageProc packageDir cmd args = (P.proc cmd args) {P.cwd = Just $ fromAbsDir packageDir}
