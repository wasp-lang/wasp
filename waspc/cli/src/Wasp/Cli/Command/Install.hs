module Wasp.Cli.Command.Install
  ( install,
    installIO,
    reinstall,
    installIfNeeded,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), ensurePackageIsAtInstallationPathInProject, getPackagePathInNodeModules)
import Wasp.Project.Common (WaspProjectDir)
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

-- | Runs install when it's certain that the `wasp-config` package won't
-- resolve correctly
--
-- Examples include:
--   - The `wasp-config` link resolves to an empty directory (e.g., if the user
--   ran `npm install` on their own)
--   - `wasp-config`'s version is different than the CLI's version.
--
-- This check can have false positivies but not false negatives:
--   - It it decides that an install is needed, it definitely is.
--   - It can decide an install isn't needed even though it is. For example,
--   imagine  a user added a dependency in their package.json, imports it in
--   main.wasp.ts, and doesn't run npm install). In such rare cases, this
--   command decides the install isn't needed, and the command the user
--   originally intended to run breaks with Node's regular "Could not resolve
--   dependency error
installIfNeeded :: Command ()
installIfNeeded = do
  InWaspProject waspProjectDir <- require
  let waspConfigInNodeModules = waspProjectDir </> getPackagePathInNodeModules WaspConfigPackage
  exists <- liftIO $ IOUtil.doesDirectoryExist waspConfigInNodeModules
  unless exists install

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspConfigPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir
