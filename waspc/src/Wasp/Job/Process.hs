module Wasp.Job.Process (run, runUntil) where

import Control.Concurrent.STM (STM)
import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job (Job, fromCallback)
import qualified Wasp.Process as Process

-- | Runs the process to completion and streams its output.
-- Stopping the job early stops the process.
run :: Process.InputMode -> P.CreateProcess -> Job ExitCode
run inputMode process = fromCallback $ Process.run inputMode process

-- | Like 'run', but also stops the process once the given transaction
-- succeeds, while still streaming what it writes as it shuts down.
runUntil :: STM () -> Process.InputMode -> P.CreateProcess -> Job ExitCode
runUntil stopRequested inputMode process = fromCallback $ Process.runUntil stopRequested inputMode process
