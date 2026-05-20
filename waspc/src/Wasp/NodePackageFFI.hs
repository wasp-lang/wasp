module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for setting up and running node processes from the
    -- @packages/@ directory.
    RunnablePackage (..),
    getPackageProcessOptions,
    InstallablePackage (..),
    getInstallablePackageName,
    getPackageJsonSpecifierForPackage,
    tryGettingInstalledPackageVersion,
    ensurePackageIsAtInstallationPathInProject,
    getInstallablePackageScriptInProject,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)
import StrongPath
  ( Abs,
    Dir,
    File,
    File',
    Path',
    Rel,
    castDir,
    castFile,
    castRel,
    fromAbsDir,
    fromAbsFile,
    fromRelDir,
    parseRelDir,
    reldir,
    relfile,
    (</>),
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as P
import Wasp.Data (DataDir)
import qualified Wasp.Data as Data
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil

-- | These are the globally installed packages waspc runs directly from
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

-- | These are globally installed packages waspc copies into a location inside
-- the user's project and then installs using `npm`'s file specifiers. They are
-- used/run from inside the project's node_modules.
data InstallablePackage = WaspSpecPackage

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
  ensurePackageDependenciesAreInstalled packageDir
  return $ packageCreateProcess packageDir "node" (fromAbsFile scriptFile : args)

getPackageJsonSpecifierForPackage :: InstallablePackage -> String
getPackageJsonSpecifierForPackage package =
  "file:" ++ fromRelDir (getPackageInstallationPathInProject package)

getInstallablePackageName :: InstallablePackage -> String
getInstallablePackageName = \case
  -- NOTE: These names must match the 'name' fields in packages' package.json files.
  WaspSpecPackage -> "@wasp.sh/spec"

ensurePackageIsAtInstallationPathInProject :: Path' Abs (Dir WaspProjectDir) -> InstallablePackage -> IO ()
ensurePackageIsAtInstallationPathInProject projectDir package = do
  let dstPackageDirInProject = projectDir </> getPackageInstallationPathInProject package
  waspDataDir <- Data.getAbsDataDirPath
  let srcPackageDir = waspDataDir </> packagesDirInDataDir </> installablePackageDirInPackagesDir package
  -- We remove the destination directory first to ensure a clean state
  IOUtil.deleteDirectoryIfExists dstPackageDirInProject
  IOUtil.copyDirectory srcPackageDir dstPackageDirInProject

getPackageInstallationPathInProject :: InstallablePackage -> Path' (Rel WaspProjectDir) (Dir d)
getPackageInstallationPathInProject package =
  dotWaspDirInWaspProjectDir </> castRel (castDir $ installablePackageDirInPackagesDir package)

tryGettingInstalledPackageVersion ::
  Path' Abs (Dir WaspProjectDir) ->
  InstallablePackage ->
  IO (Either String SV.Version)
tryGettingInstalledPackageVersion projectDir package = runExceptT $ do
  unlessM (liftIO $ IOUtil.doesFileExist packageJsonPath) $
    throwError $
      "Couldn't find " ++ fromAbsFile packageJsonPath
  packageJson <- ExceptT $ liftIO $ PJ.parsePackageJsonFile packageJsonPath
  ExceptT $ return $ case PJ.version packageJson of
    Just versionString -> first show $ SV.parseVersion versionString
    Nothing -> Left $ fromAbsFile packageJsonPath ++ " has no `version` field"
  where
    packageJsonPath :: Path' Abs (File InstalledPackageJsonFile)
    packageJsonPath =
      castFile $
        projectDir
          </> nodeModulesDirInWaspProjectDir
          </> fromJust (parseRelDir $ getInstallablePackageName package)
          </> [relfile|package.json|]

data InstalledPackageJsonFile

instance PJ.PackageJsonFile InstalledPackageJsonFile

-- | Returns the path to the main script of an installable package, relative
-- to the project root. This can be passed to @node@ directly, avoiding the
-- need for @npx@ and its requirement that bin files are executable.
getInstallablePackageScriptInProject :: InstallablePackage -> Path' (Rel WaspProjectDir) File'
getInstallablePackageScriptInProject package =
  castFile $ getPackageInstallationPathInProject package </> installablePackageScript package

installablePackageScript :: InstallablePackage -> Path' (Rel d) File'
installablePackageScript = \case
  WaspSpecPackage -> [relfile|dist/src/run.js|]

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
