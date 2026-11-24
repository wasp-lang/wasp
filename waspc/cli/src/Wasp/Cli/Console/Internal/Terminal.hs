-- | The only module that writes Wasp's messages and the jobs' output to the
-- terminal. Only "Wasp.Cli.Console" and the modules next to this one use it,
-- to build what the console hands out to the rest of the code.
--
-- We eventually want to route all terminal communication through this module,
-- making it the only one allowed to work with stdout, stdin and stderr. See
-- https://github.com/wasp-lang/wasp/issues/5493 for details.
module Wasp.Cli.Console.Internal.Terminal
  ( writeLine,
    writeRawOutput,
    isInputTerminal,
    writePrompt,
    readLine,
    isInputEOF,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (Handle, hFlush, hIsTerminalDevice, hPutStr, stderr, stdin, stdout)
import System.IO.Error (ioeGetHandle, isEOFError)
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

isInputTerminal :: IO Bool
isInputTerminal = hIsTerminalDevice stdin

-- | Writes a part of a prompt, which stays on the same line as the user's
-- answer, so it's written out right away.
writePrompt :: String -> IO ()
writePrompt text = putStr text >> hFlush stdout

-- | Reads the user's answer. Reaching the end of the input while waiting for
-- it (Ctrl+D) throws an error that 'isInputEOF' recognizes.
readLine :: IO String
readLine = getLine

isInputEOF :: IOError -> Bool
isInputEOF e = isEOFError e && ioeGetHandle e == Just stdin
