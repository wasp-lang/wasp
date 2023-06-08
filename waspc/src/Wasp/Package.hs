{-# LANGUAGE DeriveAnyClass #-}

module Wasp.Package
  ( Package (..),
    runPackageAsJob,
  )
where

import Control.Concurrent.Chan (newChan)
import Control.Monad.Extra (unlessM, void)
import StrongPath (Abs, Dir, Path', Rel, reldir, toFilePath, (</>))
import System.Directory (doesDirectoryExist)
import Wasp.Data (DataDir)
import qualified Wasp.Data as Data
import Wasp.Generator.Job (Job)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)

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

-- | Run a package from @packages/@ as a 'Job'.
--
-- These packages are built during CI/locally via the @tools/install_packages_to_data_dir.sh@
-- script.
--
-- If the package does not have its dependencies installed yet (i.e. after they
-- just installed a Wasp version), we install the dependencies.
runPackageAsJob ::
  Package ->
  -- | Command line arguments to pass to the package.
  [String] ->
  Job
runPackageAsJob package args chan = do
  waspDataDir <- Data.getAbsDataDirPath
  let packageDir = waspDataDir </> packagesDirInDataDir </> packageDirInPackagesDir package
  ensurePackageDependenciesAreInstalled packageDir
  -- NOTE: Here we are lying by saying we are running in the J.Server context.
  -- TODO: Consider adding a new context for these types of things, like J.Package.
  runNodeCommandAsJob packageDir "npm" npmStartArgs J.Server chan
  where
    -- @--@ must be put before any args that are to be passed to the command
    -- inside the npm script.
    npmStartArgs = concat [["start", "--"], args]

-- Runs @npm install@ if @node_modules@ does not exist in the package directory.
ensurePackageDependenciesAreInstalled :: Path' Abs (Dir PackageDir) -> IO ()
ensurePackageDependenciesAreInstalled packageDir =
  unlessM nodeModulesDirExists $ do
    chan <- newChan
    void $ runNodeCommandAsJob packageDir "npm" ["install"] J.Server chan
  where
    nodeModulesDirExists = doesDirectoryExist $ toFilePath nodeModulesDir
    nodeModulesDir = packageDir </> [reldir|node_modules|]
