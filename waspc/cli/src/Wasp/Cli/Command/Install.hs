module Wasp.Cli.Command.Install
  ( install,
    installIO,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Generator.NpmInstall (InstallOutputMode (..), installProjectNpmDependencies)
import qualified Wasp.Message as Msg
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir)

-- | Standalone `wasp install` command: copies @wasp/spec and runs npm install.
install :: Command ()
install = ProjectLock.acquireExclusive $ do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  liftIO (installIO ShowInstallOutput waspProjectDir)
    >>= either
      (\errors -> liftIO (cliSendMessage $ Msg.Failure "Couldn't install npm dependencies" errors) >> throwError CommandAborted)
      return

installIO :: InstallOutputMode -> Path' Abs (Dir WaspProjectDir) -> IO (Either [Msg.Part] ())
installIO installOutputMode waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  installProjectNpmDependencies installOutputMode waspProjectDir
