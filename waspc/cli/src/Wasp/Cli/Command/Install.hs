module Wasp.Cli.Command.Install
  ( install,
    installIO,
    installIfNeeded,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.NpmInstall (hasUserUpdatedNpmDeps, installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject, tryGettingInstalledPackageVersion)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.WaspFile (isWaspTsProject)
import qualified Wasp.Version as WV

-- | Standalone `wasp install` command: copies @wasp.sh/spec and runs npm install.
install :: Command ()
install = do
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

-- | Runs install when necessary. Checks two conditions:
--   1. This is a Wasp TS spec project and the @wasp.sh/spec package is missing
--   or version-mismatched in node_modules (e.g., first compile, after clean, or
--   if the user ran `npm install` on their own).
--   2. The user's dependencies in package.json have changed since the last
--   recorded install (compared against the installedNpmDepsLog).
installIfNeeded :: Command ()
installIfNeeded = do
  InWaspProject waspProjectDir <- require

  isTsProject <- liftIO $ isWaspTsProject waspProjectDir

  waspSpecOutOfDate <-
    liftIO (tryGettingInstalledPackageVersion waspProjectDir WaspSpecPackage) <&> \case
      Left _ -> True
      Right installedVersion -> installedVersion /= WV.waspVersion

  userDepsChanged <-
    liftIO (hasUserUpdatedNpmDeps waspProjectDir) >>= \case
      Left err -> throwError $ CommandError "Couldn't read user npm dependencies" err
      Right changed -> return changed

  when (isTsProject && (waspSpecOutOfDate || userDepsChanged)) install

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir
