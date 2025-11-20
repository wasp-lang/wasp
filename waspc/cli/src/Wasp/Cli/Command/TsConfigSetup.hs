module Wasp.Cli.Command.TsConfigSetup (tsConfigSetup) where

import Control.Concurrent (Chan, newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Node.Executables (npmExec)
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), getPackageInstallationPath)

-- | Prepares the project for using Wasp's TypeScript SDK.
tsConfigSetup :: Command ()
tsConfigSetup = do
  InWaspProject waspProjectDir <- require
  messageChan <- liftIO newChan
  -- NOTE: We're also installing the user's package.json dependencies here
  -- This is to provide proper IDE support for users working with the TS SDK
  -- (it needs the `wasp-config` package).
  liftIO (installWaspConfigPackage messageChan waspProjectDir)
    >>= onLeftThrowError
  where
    onLeftThrowError = either (throwError . CommandError "npm install failed") pure

installWaspConfigPackage :: Chan J.JobMessage -> Path' Abs (Dir a) -> IO (Either String ())
installWaspConfigPackage chan projectDir = do
  installationPath <- getPackageInstallationPath WaspConfigPackage
  (_, exitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (runNodeCommandAsJob projectDir npmExec ["install", "--save-dev", "file:" ++ installationPath] J.Wasp chan)
  return $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left "Failed to install wasp-config package"
