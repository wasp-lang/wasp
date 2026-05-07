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
import Wasp.Generator.NpmInstall (hasUserUpdatedNpmDeps, installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), ensurePackageIsAtInstallationPathInProject, getPackagePathInNodeModules)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.WaspFile (isWaspTsProject)
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

-- | Runs install when necessary. Checks three conditions:
--   1. This is Wasp TS spec project and the `wasp-config` package is missing
--   from node_modules (e.g., first compile, after clean, or if the user ran
--   `npm install` on their own).
--   2. The user's dependencies in package.json have changed since the last
--   recorded install (compared against the installedNpmDepsLog).
installIfNeeded :: Command ()
installIfNeeded = do
  InWaspProject waspProjectDir <- require

  isTsProject <- liftIO $ isWaspTsProject waspProjectDir

  let waspConfigInNodeModules = waspProjectDir </> getPackagePathInNodeModules WaspConfigPackage
  waspConfigMissing <- liftIO $ not <$> IOUtil.doesDirectoryExist waspConfigInNodeModules

  userDepsChanged <-
    liftIO (hasUserUpdatedNpmDeps waspProjectDir) >>= \case
      Left err -> throwError $ CommandError "Couldn't read user npm dependencies" err
      Right changed -> return changed

  when (isTsProject && (waspConfigMissing || userDepsChanged)) install

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspConfigPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir
