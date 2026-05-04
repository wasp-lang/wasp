module Wasp.Cli.Command.TsConfigSetup (tsConfigSetup) where

import Control.Concurrent (newChan)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), ensurePackageIsAtInstallationPathInProject)

-- | Prepares the project for using Wasp's TypeScript SDK.
tsConfigSetup :: Command ()
tsConfigSetup = do
  InWaspProject waspProjectDir <- require
  liftIO $ ensurePackageIsAtInstallationPathInProject waspProjectDir WaspConfigPackage
  messageChan <- liftIO newChan
  liftIO (installProjectNpmDependencies messageChan waspProjectDir)
    >>= onLeftThrowError
  where
    onLeftThrowError = either (throwError . CommandError "npm install failed") pure
