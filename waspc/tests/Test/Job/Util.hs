module Test.Job.Util
  ( writeOutputToStdout,
  )
where

import qualified Data.Text.IO as T.IO
import Wasp.Util.IO.Process (WriteOutput)

-- | Writes all of the jobs' output to the test's stdout.
writeOutputToStdout :: WriteOutput
writeOutputToStdout _outputStream = T.IO.putStr
