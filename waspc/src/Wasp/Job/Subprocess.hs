module Wasp.Job.Subprocess
  ( run,
    runReturningExitCode,
  )
where

import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job.Internal (JobAction, requireExitSuccess)
import qualified Wasp.Job.Subprocess.Finite as Finite

-- | Runs the process to completion, failing the Job on a nonzero child exit.
run :: P.CreateProcess -> JobAction ()
run process = runReturningExitCode process >>= requireExitSuccess

-- | Runs the process to completion and returns its exit status for explicit handling.
runReturningExitCode :: P.CreateProcess -> JobAction ExitCode
runReturningExitCode = Finite.run
