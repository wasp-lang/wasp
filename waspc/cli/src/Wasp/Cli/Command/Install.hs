module Wasp.Cli.Command.Install
  ( install,
    installIO,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import qualified Wasp.Message as Msg
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

-- | Standalone `wasp install` command: copies @wasp.sh/spec and runs npm install.
install :: Command ()
install = do
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  removeStaleGeneratedAppDir waspProjectDir
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir

removeStaleGeneratedAppDir :: Path' Abs (Dir WaspProjectDir) -> IO ()
removeStaleGeneratedAppDir waspProjectDir = do
  let generatedAppDir = waspProjectDir </> generatedAppDirInWaspProjectDir
  generatedAppDirExists <- IOUtil.doesDirectoryExist generatedAppDir
  when generatedAppDirExists $ do
    wasGeneratedByCurrentVersion <- either (const False) WaspInfo.isGeneratedByCurrentVersion <$> WaspInfo.safeRead generatedAppDir
    unless wasGeneratedByCurrentVersion $ do
      cliSendMessage $ Msg.Start "Removing stale .wasp/out before npm install..."
      IOUtil.deleteDirectoryIfExists generatedAppDir
      cliSendMessage $ Msg.Success "Removed stale .wasp/out."
