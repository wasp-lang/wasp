module Wasp.Job.Node (run, runUntil) where

import Control.Concurrent.STM (STM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import qualified System.Process as P
import Wasp.Job (Job)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Process as JobProcess
import Wasp.Process (InputMode, OutputStream (Stderr))
import qualified Wasp.Process.Node as NodeProcess

-- | Runs the command to completion after checking the user's Node and npm
-- versions. Exits with code 1 if they don't meet Wasp's requirements.
run :: InputMode -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job ExitCode
run inputMode = runCommandUsing $ JobProcess.run inputMode

-- | Like 'run', but also stops the command once the given transaction succeeds.
runUntil :: STM () -> InputMode -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job ExitCode
runUntil stopRequested inputMode = runCommandUsing $ JobProcess.runUntil stopRequested inputMode

runCommandUsing :: (P.CreateProcess -> Job ExitCode) -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job ExitCode
runCommandUsing runProcess extraEnvVars workingDir executable arguments = do
  prepared <- liftIO $ NodeProcess.prepare extraEnvVars workingDir executable arguments
  case prepared of
    Left message -> do
      Job.emit Stderr $ T.pack message
      return $ ExitFailure 1
    Right process -> runProcess process
