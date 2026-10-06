module Wasp.Cli.Command.Install
  ( install,
    installIO,
    LockfileHandling (..),
  )
where

import Control.Concurrent (newChan)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.Generator.NpmInstall (installProjectNpmDependencies, installProjectNpmDependenciesWithoutSavingLockfile)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir, packageLockJsonInWaspProjectDir)
import Wasp.Util.IO (doesFileExist)

-- | Standalone `wasp install` command: copies @wasp.sh/spec and runs npm install.
install :: Command ()
install = withProjectLock $ do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  liftIO (installIO UpdateLockfile waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

-- | What 'installIO' may do to the project's `package-lock.json`.
data LockfileHandling
  = -- | Run a plain `npm install`, which may rewrite the lockfile. If the
    -- generated code is missing, npm prunes its workspaces' entries from the
    -- lockfile (https://github.com/wasp-lang/wasp/issues/4482), but it also
    -- repairs stale entries, like an outdated integrity of a Wasp lib tarball.
    UpdateLockfile
  | -- | Run `npm install --no-save` if the lockfile exists, so it stays
    -- untouched (see 'installProjectNpmDependenciesWithoutSavingLockfile').
    -- Without a lockfile, run a plain `npm install`: with `--no-save`, the
    -- lockfile that the post-generation install creates would miss integrity
    -- hashes.
    KeepExistingLockfile

-- | Copies @wasp.sh/spec into the project and installs the project's npm
-- dependencies.
installIO :: LockfileHandling -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO lockfileHandling waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  messageChan <- newChan
  installNpmDependencies <- case lockfileHandling of
    UpdateLockfile -> return installProjectNpmDependencies
    KeepExistingLockfile -> do
      isLockfilePresent <- doesFileExist $ waspProjectDir </> packageLockJsonInWaspProjectDir
      return $
        if isLockfilePresent
          then installProjectNpmDependenciesWithoutSavingLockfile
          else installProjectNpmDependencies
  installNpmDependencies messageChan waspProjectDir
