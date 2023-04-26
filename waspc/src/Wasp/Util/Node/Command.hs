module Wasp.Util.Node.Command where

import System.Exit (ExitCode)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    shell,
    std_out,
    waitForProcess,
  )
import UnliftIO.Exception (SomeException, try)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.Node.Version as NodeVersion

runNodeCommandSilenced :: [String] -> IO (Either String ExitCode)
runNodeCommandSilenced command =
  NodeVersion.getNodeVersion >>= \case
    Left nodeVersionErrorMsg -> return $ Left nodeVersionErrorMsg
    Right nodeVersion ->
      if SV.isVersionInRange nodeVersion NodeVersion.nodeVersionRange
        then do
          try executeCommand >>= \case
            Left (e :: SomeException) -> return (Left $ show e)
            Right exitCode -> return $ Right exitCode
        else return $ Left (NodeVersion.makeNodeVersionMismatchMessage nodeVersion)
  where
    executeCommand :: IO ExitCode
    executeCommand = do
      -- Creating a pipe for stdout to disable printing of the command output.
      (_, _, _, processHandle) <- createProcess (shell $ unwords command) {std_out = CreatePipe, std_err = CreatePipe}
      waitForProcess processHandle
