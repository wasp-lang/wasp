module Wasp.Job.Subprocess
  ( run,
  )
where

import System.Exit (ExitCode)
import qualified System.Process as P
import Wasp.Job.Internal (JobAction)
import qualified Wasp.Job.Subprocess.Finite as Finite

run :: P.CreateProcess -> JobAction ExitCode
run = Finite.run
