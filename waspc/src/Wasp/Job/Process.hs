module Wasp.Job.Process (runChecked, runReturningExitCode) where

import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job (Job, getJobOutputSink, requireExitSuccess)
import qualified Wasp.Job.Output.Event as Event
import qualified Wasp.Process as Process

-- | Fails the job on a nonzero child exit.
runChecked :: P.CreateProcess -> Job ()
runChecked process = runReturningExitCode process >>= requireExitSuccess

runReturningExitCode :: P.CreateProcess -> Job ExitCode
runReturningExitCode process = do
  emit <- getJobOutputSink
  liftIO $ Process.run process $ \stream -> emit $ case stream of
    Process.Stdout -> Event.Stdout
    Process.Stderr -> Event.Stderr
