module Wasp.Job.Process (run) where

import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job (Job, fromCallback)
import qualified Wasp.Process as Process

-- | Runs the process to completion and streams its output.
-- Stopping the job early stops the process.
run :: Process.InputMode -> P.CreateProcess -> Job ExitCode
run inputMode process = fromCallback $ Process.run inputMode process
