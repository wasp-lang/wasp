module Wasp.Cli.Command.Install
  ( install,
    installIfNeeded,
    installIO,
    installingDependenciesMessage,
  )
where

import Control.Concurrent (newChan)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (isInstalledWaspSpecUpToDate)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Generator.NpmInstall (installProjectNpmDependencies, installProjectNpmDependenciesWithoutSavingLockfile)
import qualified Wasp.Message as Msg
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir, packageLockJsonInWaspProjectDir)
import Wasp.Util (unlessM)
import Wasp.Util.IO (doesFileExist)

-- | Standalone `wasp install` command: copies @wasp.sh/spec and runs npm install.
install :: Command ()
install = ProjectLock.acquireExclusive installProjectDependencies

-- | Runs `wasp install` if @wasp.sh/spec is missing or doesn't match the CLI
-- version. Installing changes `node_modules`, so the caller must hold the
-- project lock.
installIfNeeded :: Command ()
installIfNeeded = do
  InWaspProject waspProjectDir <- require
  unlessM (liftIO $ isInstalledWaspSpecUpToDate waspProjectDir) $ do
    cliSendMessageC $ Msg.Start installingDependenciesMessage
    installProjectDependencies

installingDependenciesMessage :: String
installingDependenciesMessage = "Installing missing or outdated project dependencies..."

-- | Copies @wasp.sh/spec into the project and runs npm install.
installProjectDependencies :: Command ()
installProjectDependencies = do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

-- | Copies @wasp.sh/spec into the project and installs its npm dependencies.
-- Keeps an existing `package-lock.json` as is, and the post-generation install
-- updates it later. Without a lockfile, it runs a plain install, because
-- `--no-save` would leave the lockfile created later without integrity hashes.
installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  messageChan <- newChan
  isLockfilePresent <- doesFileExist $ waspProjectDir </> packageLockJsonInWaspProjectDir
  let installNpmDependencies =
        if isLockfilePresent
          then installProjectNpmDependenciesWithoutSavingLockfile
          else installProjectNpmDependencies
  installNpmDependencies messageChan waspProjectDir
