module Wasp.Cli.Command.Install
  ( install,
    installIO,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, commandError, getConsole, require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Console (Console (..))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.IO.Process (WriteOutput)

-- | Standalone `wasp install` command: copies @wasp/spec and runs npm install.
install :: Command ()
install = ProjectLock.acquireExclusive $ do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  console <- getConsole
  liftIO (installIO console.writeRawOutput waspProjectDir)
    >>= either
      (throwError . commandError "Couldn't install npm dependencies")
      return

installIO :: WriteOutput -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO writeOutput waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  installProjectNpmDependencies writeOutput waspProjectDir
