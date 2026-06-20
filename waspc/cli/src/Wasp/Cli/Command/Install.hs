module Wasp.Cli.Command.Install
  ( install,
    installIO,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.IO (hPutStrLn, stderr)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.ExternalConfig.Npm.Lockfile (stripStaleWaspEntries)
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir, packageLockJsonInWaspProjectDir)
import Wasp.Util.IO (doesFileExist)
import Wasp.Util.Json (updateJsonFile)

-- | Standalone `wasp install` command: copies @wasp.sh/spec, cleans stale
-- lockfile entries, then runs npm install.
install :: Command ()
install = do
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  -- Remove stale @wasp.sh/lib-* and generated-workspace entries from
  -- package-lock.json so npm install doesn't fail on old tarball references
  -- after a Wasp version bump.
  cleanLockFile waspProjectDir
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir

-- | Remove stale Wasp-generated entries from package-lock.json before install.
-- If the file doesn't exist (first-time setup), skip it silently.
cleanLockFile :: Path' Abs (Dir WaspProjectDir) -> IO ()
cleanLockFile waspProjectDir = do
  let lockFilePath = waspProjectDir SP.</> packageLockJsonInWaspProjectDir
  exists <- doesFileExist lockFilePath
  when exists $ do
    result <- updateJsonFile stripStaleWaspEntries lockFilePath
    case result of
      Left err -> hPutStrLn stderr $ "Warning: Failed to clean package-lock.json: " ++ err
      Right _ -> return ()
