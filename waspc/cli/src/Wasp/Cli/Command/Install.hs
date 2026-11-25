module Wasp.Cli.Command.Install
  ( install,
    installIO,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, commandErrorWithToolOutput, require)
import Wasp.Cli.Command.Message (runTaskC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Problem (ToolOutput)
import Wasp.Project.Common (WaspProjectDir)

-- | Standalone `wasp install` command: copies @wasp/spec and runs npm install.
install :: Command ()
install = ProjectLock.acquireExclusive $ do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  runTaskC "Installing dependencies" "Installed dependencies" "Failed to install dependencies" $
    liftIO (installIO waspProjectDir)
      >>= either
        (throwError . commandErrorWithToolOutput "Installing the project's dependencies failed")
        return

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either ToolOutput ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  installProjectNpmDependencies waspProjectDir
