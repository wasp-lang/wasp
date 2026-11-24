module Wasp.Cli.Console
  ( Console (..),
    initConsole,
  )
where

import qualified Data.Text.IO as T.IO
import System.IO (Handle, hFlush, stderr, stdout)
import qualified Wasp.Job as Job
import Wasp.Util.IO.Process (OutputStream (..), WriteOutput)

-- | Everything the CLI uses to talk to the user. There is a single one, created
-- at startup and handed down to the code that needs it.
newtype Console = Console
  { -- | Where the jobs write the output that the user should see.
    writeRawOutput :: WriteOutput
  }

initConsole :: IO Console
initConsole = return Console {writeRawOutput = writeJobOutput}

writeJobOutput :: WriteOutput
writeJobOutput outputStream output = T.IO.hPutStr outputHandle output >> hFlush outputHandle
  where
    outputHandle = getOutputStreamHandle outputStream

getOutputStreamHandle :: OutputStream -> Handle
getOutputStreamHandle Stdout = stdout
getOutputStreamHandle Stderr = stderr
