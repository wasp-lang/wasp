module Wasp.Cli.Command.Install
  ( install,
    installIO,
    reinstall,
    installIfNeeded,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.NpmDependencies (NpmDepsFromUser, getUserNpmDepsForPackage)
import Wasp.Generator.NpmInstall (areThereUserNpmDepsToInstall, installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), ensurePackageIsAtInstallationPathInProject, getPackagePathInNodeModules)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Project.ExternalConfig.PackageJson (readPackageJsonFile)
import qualified Wasp.Util.IO as IOUtil

-- | Standalone `wasp install` command: copies wasp-config and runs npm install.
install :: Command ()
install = do
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

reinstall :: Command ()
reinstall = clean >> install

-- | Runs install when necessary. Checks two conditions:
--   1. The `wasp-config` package is missing from node_modules (e.g., first
--      compile, after clean, or if the user ran `npm install` on their own).
--   2. The user's dependencies in package.json have changed since the last
--      recorded install (compared against the installedNpmDepsLog).
installIfNeeded :: Command ()
installIfNeeded = do
  InWaspProject waspProjectDir <- require
  let waspConfigInNodeModules = waspProjectDir </> getPackagePathInNodeModules WaspConfigPackage
  let outDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir

  waspConfigMissing <- liftIO $ not <$> IOUtil.doesDirectoryExist waspConfigInNodeModules
  userDepsChanged <-
    liftIO $
      getUserDepsFromDisk waspProjectDir >>= \case
        Left _ -> pure True
        Right userDeps -> areThereUserNpmDepsToInstall userDeps outDir

  when (waspConfigMissing || userDepsChanged) install

getUserDepsFromDisk :: Path' Abs (Dir WaspProjectDir) -> IO (Either String NpmDepsFromUser)
getUserDepsFromDisk waspProjectDir =
  fmap getUserNpmDepsForPackage <$> readPackageJsonFile waspProjectDir

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspConfigPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir
