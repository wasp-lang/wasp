module Wasp.Job.Node
  ( runChecked,
    runReturningExitCode,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job (failWithExitCode)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output.Event as Event
import qualified Wasp.Job.Process as JobProcess
import Wasp.Process (InputMode)
import qualified Wasp.Process.Node as NodeProcess

-- | Runs the command to completion, failing the Job on a nonzero child exit.
runChecked :: InputMode -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.Job ()
runChecked inputMode = runCommandUsing $ JobProcess.runChecked inputMode

-- | Runs the command and returns the child process's exit status for explicit handling.
runReturningExitCode :: InputMode -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.Job ExitCode
runReturningExitCode inputMode = runCommandUsing $ JobProcess.runReturningExitCode inputMode

runCommandUsing :: (P.CreateProcess -> Job.Job a) -> [(String, String)] -> Path' Abs (Dir dir) -> String -> [String] -> Job.Job a
runCommandUsing runProcess extraEnvVars workingDir executable arguments = do
  prepared <- liftIO $ NodeProcess.prepare extraEnvVars workingDir executable arguments
  case prepared of
    Left message -> do
      Job.emitJobOutput Event.Stderr $ T.pack message
      failWithExitCode 1
    Right process -> runProcess process
