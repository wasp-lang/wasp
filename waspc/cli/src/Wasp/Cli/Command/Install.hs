module Wasp.Cli.Command.Install
  ( install,
    waspInstallIfNeeded,
    installIO,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (isWaspSpecAvailable)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import qualified Wasp.Message as Msg
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir)

-- | Standalone `wasp install` command: copies @wasp.sh/spec and runs npm install.
install :: Command ()
install = ProjectLock.acquireExclusive waspInstallWithoutLock

-- | Runs `wasp install` if @wasp.sh/spec is missing or doesn't match the CLI
-- version. Installing changes `node_modules`, so the caller must hold the
-- project lock.
waspInstallIfNeeded :: Command ()
waspInstallIfNeeded = do
  InWaspProject waspProjectDir <- require
  isAvailable <- liftIO $ isWaspSpecAvailable waspProjectDir
  unless isAvailable $ do
    cliSendMessageC $ Msg.Start "Installing missing or outdated project dependencies..."
    waspInstallWithoutLock

waspInstallWithoutLock :: Command ()
waspInstallWithoutLock = do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir
