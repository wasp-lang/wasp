module Wasp.Cli.Command.Install
  ( install,
    installIO,
    parserInfo,
  )
where

import Control.Concurrent (newChan)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Definition (CommandParserInfo, command)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject)
import Wasp.Project.Common (WaspProjectDir)

parserInfo :: CommandParserInfo
parserInfo = command "Set up all internal Wasp npm dependencies and run npm install." install

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
  ensurePackageIsAtInstallationPathInProject waspProjectDir WaspSpecPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan waspProjectDir
