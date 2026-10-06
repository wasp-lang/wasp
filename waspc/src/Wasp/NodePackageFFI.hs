module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for setting up and running node processes from the
    -- @packages/@ directory.
    RunnablePackage (..),
    getPackageProcessOptions,
    InstallablePackage (..),
    getInstallablePackageName,
    getPackageJsonSpecifierForPackage,
    getInstallablePackageScript,
    ensurePackageIsAtInstallationPathInProject,
    ensurePackageInProjectMatchesWaspVersion,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import StrongPath
  ( Abs,
    Dir,
    File,
    File',
    Path',
    Rel,
    castDir,
    castRel,
    fromAbsDir,
    fromAbsFile,
    fromRelDir,
    reldir,
    relfile,
    (</>),
  )
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as P
import Wasp.Data (DataDir)
import qualified Wasp.Data as Data
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Version as WV

-- | These are the globally installed packages waspc runs directly from
-- their global installation path.
data RunnablePackage
  = DeployPackage
  | -- | TODO(martin): I implemented this ts package because I planned to use prisma's TS sdk
    --   (@prisma/internals) inside it, but I ended up calling `prisma format` cli cmd directly,
    --   which means I could have really done it from Haskell!
    --   Therefore, reconsider if we should have this package, or if we should delete it and move
    --   this functionality here, into Haskell.
    --   It might make sense to keep it we will be maybe using @prisma/internals or some other
    --   prisma packages via it in the future, if not then it is not worth keeping it.
    PrismaPackage
  | WaspStudioPackage

-- | These are globally installed packages waspc runs directly from their global
-- installation path, but on the user's project (see 'getInstallablePackageScript').
--
-- waspc also copies them into a location inside the user's project, which the
-- project installs using `npm`'s file specifiers. That copy is for the user's
-- own tooling (e.g., the editor's types for `@wasp.sh/spec`), waspc doesn't run it.
data InstallablePackage = WaspSpecPackage

data PackagesDir

data PackageDir

data PackageScript

packagesDirInDataDir :: Path' (Rel DataDir) (Dir PackagesDir)
packagesDirInDataDir = [reldir|packages|]

runnablePackageDirInPackagesDir :: RunnablePackage -> Path' (Rel PackagesDir) (Dir PackageDir)
runnablePackageDirInPackagesDir = \case
  DeployPackage -> [reldir|deploy|]
  PrismaPackage -> [reldir|prisma|]
  WaspStudioPackage -> [reldir|studio|]

installablePackageDirInPackagesDir :: InstallablePackage -> Path' (Rel PackagesDir) (Dir PackageDir)
installablePackageDirInPackagesDir = \case
  WaspSpecPackage -> [reldir|spec|]

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
  ensurePackageDependenciesAreInstalled npmInstallAllDependenciesArgs packageDir
  return $ packageCreateProcess packageDir "node" (fromAbsFile scriptFile : args)

-- | Returns the absolute path of the package's main script in its global
-- installation path, installing the package's dependencies there first if
-- needed (for example, when the package is run for the first time after
-- installing Wasp).
--
-- Unlike 'getPackageProcessOptions', it leaves the process setup (e.g., the
-- working directory) to the caller, since these packages work on the user's
-- project. The script can be passed to @node@ directly, avoiding the need for
-- @npx@ and its requirement that bin files are executable (`cabal install`
-- strips executable permissions from data files).
getInstallablePackageScript :: InstallablePackage -> IO (Path' Abs File')
getInstallablePackageScript package = do
  packageDir <- getInstallablePackageDir package
  ensurePackageDependenciesAreInstalled npmInstallRuntimeDependenciesArgs packageDir
  return $ packageDir </> installablePackageScript package

getPackageJsonSpecifierForPackage :: InstallablePackage -> String
getPackageJsonSpecifierForPackage package =
  "file:" ++ fromRelDir (getPackageInstallationPathInProject package)

getInstallablePackageName :: InstallablePackage -> String
getInstallablePackageName = \case
  -- NOTE: These names must match the 'name' fields in packages' package.json files.
  WaspSpecPackage -> "@wasp.sh/spec"

-- | Copies the package into its installation path in the project, replacing
-- whatever was there.
ensurePackageIsAtInstallationPathInProject :: Path' Abs (Dir WaspProjectDir) -> InstallablePackage -> IO ()
ensurePackageIsAtInstallationPathInProject projectDir package = do
  srcPackageDir <- getInstallablePackageDir package
  let dstPackageDir = projectDir </> getPackageInstallationPathInProject package
  -- We remove the destination directory first to ensure a clean state. We
  -- never merge into it: e.g., `npm install` on a fresh clone creates it empty.
  IOUtil.deleteDirectoryIfExists dstPackageDir
  createDirectoryIfMissing True $ fromAbsDir dstPackageDir
  -- We copy only what the published Wasp CLI ships for the package (see
  -- `data-files` in `waspc.cabal`). Notably, we skip its `node_modules`, which
  -- exists in the global installation path once the package has run.
  IOUtil.copyDirectory (srcPackageDir </> [reldir|dist|]) (dstPackageDir </> [reldir|dist|])
  mapM_
    (\file -> IOUtil.copyFile (srcPackageDir </> file) (dstPackageDir </> file))
    [[relfile|package.json|], [relfile|package-lock.json|]]

-- | Like 'ensurePackageIsAtInstallationPathInProject', but only copies the
-- package if the project's copy is missing or its version differs from this
-- Wasp's version. This makes it cheap to call on every analysis (e.g., on each
-- recompile in `wasp start`), without rewriting files the user's editor reads.
ensurePackageInProjectMatchesWaspVersion :: Path' Abs (Dir WaspProjectDir) -> InstallablePackage -> IO ()
ensurePackageInProjectMatchesWaspVersion projectDir package =
  tryGettingPackageVersionInProject projectDir package >>= \case
    Right packageVersion | packageVersion == WV.waspVersion -> return ()
    _ -> ensurePackageIsAtInstallationPathInProject projectDir package

getPackageInstallationPathInProject :: InstallablePackage -> Path' (Rel WaspProjectDir) (Dir d)
getPackageInstallationPathInProject package =
  dotWaspDirInWaspProjectDir </> castRel (castDir $ installablePackageDirInPackagesDir package)

tryGettingPackageVersionInProject ::
  Path' Abs (Dir WaspProjectDir) ->
  InstallablePackage ->
  IO (Either String SV.Version)
tryGettingPackageVersionInProject projectDir package = runExceptT $ do
  unlessM (liftIO $ IOUtil.doesFileExist packageJsonPath)
    $ throwError
    $ "Couldn't find " ++ fromAbsFile packageJsonPath
  packageJson <- ExceptT $ liftIO $ PJ.parsePackageJsonFile packageJsonPath
  ExceptT $ return $ case PJ.version packageJson of
    Just versionString -> first show $ SV.parseVersion versionString
    Nothing -> Left $ fromAbsFile packageJsonPath ++ " has no `version` field"
  where
    packageJsonPath :: Path' Abs (File PackageJsonInProjectFile)
    packageJsonPath =
      projectDir
        </> getPackageInstallationPathInProject package
        </> [relfile|package.json|]

data PackageJsonInProjectFile

instance PJ.PackageJsonFile PackageJsonInProjectFile

installablePackageScript :: InstallablePackage -> Path' (Rel PackageDir) File'
installablePackageScript = \case
  WaspSpecPackage -> [relfile|dist/src/run.js|]

getRunnablePackageDir :: RunnablePackage -> IO (Path' Abs (Dir PackageDir))
getRunnablePackageDir package = do
  waspDataDir <- Data.getAbsDataDirPath
  let packageDir = waspDataDir </> packagesDirInDataDir </> runnablePackageDirInPackagesDir package
  return packageDir

getInstallablePackageDir :: InstallablePackage -> IO (Path' Abs (Dir PackageDir))
getInstallablePackageDir package = do
  waspDataDir <- Data.getAbsDataDirPath
  return $ waspDataDir </> packagesDirInDataDir </> installablePackageDirInPackagesDir package

npmInstallAllDependenciesArgs :: [String]
npmInstallAllDependenciesArgs = ["install"]

-- | Installs exactly the locked versions of the package's runtime
-- dependencies, without its dev tooling.
npmInstallRuntimeDependenciesArgs :: [String]
npmInstallRuntimeDependenciesArgs = ["ci", "--omit=dev", "--no-audit", "--no-fund"]

-- | Runs @npm@ with the given args if @node_modules@ does not exist in the
-- package directory.
ensurePackageDependenciesAreInstalled :: [String] -> Path' Abs (Dir PackageDir) -> IO ()
ensurePackageDependenciesAreInstalled npmArgs packageDir =
  unlessM nodeModulesDirExists $ do
    let npmInstallCreateProcess = packageCreateProcess packageDir "npm" npmArgs
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
