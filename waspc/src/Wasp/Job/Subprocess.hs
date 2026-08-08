module Wasp.Job.Subprocess
  ( Subprocess,
    run,
    runReturningExitCode,
    spawn,
    wait,
    poll,
    stop,
  )
where

import Control.Monad.Trans.Resource (ReleaseKey, allocate, release)
import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job.Internal (JobAction, getJobOutputSink, requireExitSuccess)
import qualified Wasp.Job.Subprocess.Finite as Finite
import qualified Wasp.Job.Subprocess.Managed as Managed

data Subprocess = Subprocess ReleaseKey Managed.ManagedSubprocess

-- | Runs the process to completion, failing the Job on a nonzero child exit.
run :: P.CreateProcess -> JobAction ()
run process = runReturningExitCode process >>= requireExitSuccess

-- | Runs the process to completion and returns its exit status for explicit handling.
runReturningExitCode :: P.CreateProcess -> JobAction ExitCode
runReturningExitCode = Finite.run

spawn :: P.CreateProcess -> JobAction Subprocess
spawn createProcess = do
  outputSink <- getJobOutputSink
  (releaseKey, subprocess) <- allocate (Managed.start createProcess outputSink) Managed.stop
  return $ Subprocess releaseKey subprocess

wait :: Subprocess -> IO ExitCode
wait (Subprocess _ subprocess) = Managed.waitForRootExit subprocess

poll :: Subprocess -> IO (Maybe ExitCode)
poll (Subprocess _ subprocess) = Managed.pollRootExit subprocess

stop :: Subprocess -> JobAction ()
stop (Subprocess releaseKey _) = release releaseKey
