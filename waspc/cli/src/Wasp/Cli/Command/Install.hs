module Wasp.Cli.Command.Install
  ( install,
    installIO,
    installArgsParser,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir)

-- | Standalone `wasp install` command: copies @wasp/spec and runs npm install.
install :: Arguments -> Command ()
install = withArguments installArgsParser $ \() -> ProjectLock.acquireExclusive $ do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  liftIO (installIO waspProjectDir)
    >>= either
      (throwError . CommandError "Couldn't install npm dependencies")
      return

-- | Takes no arguments, but exists so that other commands can refer to this one
-- by its name and `--help` works like for every other command.
installArgsParser :: ArgsParser ()
installArgsParser = ArgsParser "wasp install" $ pure ()

installIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installIO waspProjectDir = do
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  installProjectNpmDependencies waspProjectDir
