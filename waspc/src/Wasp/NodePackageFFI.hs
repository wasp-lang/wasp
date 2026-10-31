module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for running the TypeScript packages waspc ships in
    -- its data dir as separate Node.js processes.
    RunnablePackage (..),
    getPackageProcessOptions,
  )
where

import Control.Monad.Extra (unlessM)
import StrongPath
  ( Abs,
    Dir,
    File,
    Path',
    Rel,
    fromAbsDir,
    fromAbsFile,
    reldir,
    relfile,
    (</>),
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as P
import Wasp.Data (DataDir)
import qualified Wasp.Data as Data
import qualified Wasp.Node.Version as NodeVersion
import qualified Wasp.Util.IO as IOUtil

-- | These are the globally installed packages waspc runs directly from
-- their global installation path.
data RunnablePackage
  = DeployPackage
  | WaspStudioPackage

data PackagesDir

data PackageDir

data PackageScript

packagesDirInDataDir :: Path' (Rel DataDir) (Dir PackagesDir)
packagesDirInDataDir = [reldir|packages|]

runnablePackageDirInPackagesDir :: RunnablePackage -> Path' (Rel PackagesDir) (Dir PackageDir)
runnablePackageDirInPackagesDir = \case
  DeployPackage -> [reldir|deploy|]
  WaspStudioPackage -> [reldir|studio|]

scriptInPackageDir :: Path' (Rel PackageDir) (File PackageScript)
scriptInPackageDir = [relfile|dist/index.js|]

-- | Get a 'P.CreateProcess' for a particular package.
--
-- These packages are built during CI/locally via the @./run build:packages@
-- script.
--
-- If the package does not have its dependencies installed yet (for example,
-- when the package is run for the first time after installing Wasp), we install
-- the dependencies.
getPackageProcessOptions :: RunnablePackage -> [String] -> IO P.CreateProcess
getPackageProcessOptions package args = do
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    Left errorMsg -> do
      hPutStrLn stderr errorMsg
      exitFailure
    Right () -> pure ()

  packageDir <- getRunnablePackageDir package
  let scriptFile = packageDir </> scriptInPackageDir
  ensurePackageDependenciesAreInstalled packageDir
  return $ packageCreateProcess packageDir "node" (fromAbsFile scriptFile : args)

getRunnablePackageDir :: RunnablePackage -> IO (Path' Abs (Dir PackageDir))
getRunnablePackageDir package = do
  waspDataDir <- Data.getAbsDataDirPath
  let packageDir = waspDataDir </> packagesDirInDataDir </> runnablePackageDirInPackagesDir package
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
    nodeModulesDirExists = IOUtil.doesDirectoryExist nodeModulesDir
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
