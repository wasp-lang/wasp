module Wasp.Cli.Command.Install
  ( install,
    installIO,
  )
where

import Control.Concurrent (newChan)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Require.InLockedWaspProject (InLockedWaspProject (InLockedWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir)

-- | Standalone `wasp install` command: copies @wasp.sh/spec and runs npm install.
install :: Command ()
install = do
  ValidNodeAndNpm <- require
  InLockedWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir
