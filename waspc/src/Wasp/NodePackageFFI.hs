{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for setting up and running node processes from the
    -- @packages/@ directory.
    RunnablePackage (..),
    InstallablePackage (..),
    getPackageProcessOptions,
    getPackageInstallationPath,
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
import qualified Wasp.Node.Version as NodeVersion

-- | This are the globally installed packages waspc runs directly from
-- their global installation path.
data RunnablePackage
  = DeployPackage
  | TsInspectPackage
  | -- | TODO(martin): I implemented this ts package because I planned to use prisma's TS sdk
    --   (@prisma/internals) inside it, but I ended up calling `prisma format` cli cmd directly,
    --   which means I could have really done it from Haskell!
    --   Therefore, reconsider if we should have this package, or if we should delete it and move
    --   this functionality here, into Haskell.
    --   It might make sense to keep it we will be maybe using @prisma/internals or some other
    --   prisma packages via it in the future, if not then it is not worth keeping it.
    PrismaPackage
  | WaspStudioPackage

-- | This are the globally installed packages waspc installs into
-- the user's project using `npm`. They are used/run from inside the project's
-- node_modules.
data InstallablePackage = WaspConfigPackage

data PackagesDir

data PackageDir

data PackageScript

packagesDirInDataDir :: Path' (Rel DataDir) (Dir PackagesDir)
packagesDirInDataDir = [reldir|packages|]

runnablePackageDirInPackagesDir :: RunnablePackage -> Path' (Rel PackagesDir) (Dir PackageDir)
runnablePackageDirInPackagesDir = \case
  DeployPackage -> [reldir|deploy|]
  TsInspectPackage -> [reldir|ts-inspect|]
  PrismaPackage -> [reldir|prisma|]
  WaspStudioPackage -> [reldir|studio|]

installablePackageDirInPackagesDir :: InstallablePackage -> Path' (Rel PackagesDir) (Dir PackageDir)
installablePackageDirInPackagesDir = \case
  WaspConfigPackage -> [reldir|wasp-config|]

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
    NodeVersion.VersionCheckFail errorMsg -> do
      hPutStrLn stderr errorMsg
      exitFailure
    NodeVersion.VersionCheckSuccess -> pure ()

  packageDir <- getRunnablePackageDir package
  let scriptFile = packageDir </> scriptInPackageDir
  ensurePackageDependenciesAreInstalled packageDir
  return $ packageCreateProcess packageDir "node" (fromAbsFile scriptFile : args)

getPackageInstallationPath :: InstallablePackage -> IO String
getPackageInstallationPath package = do
  waspDataDir <- Data.getAbsDataDirPath
  let absPackagePath = waspDataDir </> packagesDirInDataDir </> installablePackageDirInPackagesDir package
  return $ fromAbsDir absPackagePath

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
