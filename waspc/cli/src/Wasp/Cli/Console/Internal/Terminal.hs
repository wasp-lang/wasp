-- | The only module that writes Wasp's messages and the jobs' output to the
-- terminal. Only "Wasp.Cli.Console" and the modules next to this one use it,
-- to build what the console hands out to the rest of the code.
module Wasp.Cli.Console.Internal.Terminal
  ( writeLine,
    writeRawOutput,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (Handle, hFlush, stderr, stdout)
import Wasp.Util.IO.Process (OutputStream (..))

writeLine :: String -> IO ()
writeLine = putStrLn

writeRawOutput :: OutputStream -> T.Text -> IO ()
writeRawOutput outputStream output = T.IO.hPutStr outputHandle output >> hFlush outputHandle
  where
    outputHandle = getOutputStreamHandle outputStream

getOutputStreamHandle :: OutputStream -> Handle
getOutputStreamHandle Stdout = stdout
getOutputStreamHandle Stderr = stderr
