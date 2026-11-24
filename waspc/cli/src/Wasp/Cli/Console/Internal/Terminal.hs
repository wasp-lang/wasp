-- | The only module that works with stdout, stdin and stderr. Everything Wasp
-- writes to the terminal or reads from it goes through here. Only
-- "Wasp.Cli.Console" and the modules next to this one use it, to build what
-- the console hands out to the rest of the code.
--
-- A lint rule in @.hlint.yaml@ keeps the other modules away from the three
-- handles and from the functions that use them without naming them (e.g.
-- 'putStrLn'). See https://github.com/wasp-lang/wasp/issues/5493 for details.
module Wasp.Cli.Console.Internal.Terminal
  ( writeLine,
    writeToStdout,
    writeRawOutput,
    isInputTerminal,
    isOutputTerminal,
    setLineBufferingForOutput,
    writePrompt,
    readLine,
    isInputEOF,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (BufferMode (LineBuffering), Handle, hFlush, hIsTerminalDevice, hSetBuffering, stderr, stdin, stdout)
import System.IO.Error (ioeGetHandle, isEOFError)
import Wasp.Util.IO.Process (OutputStream (..))

writeLine :: String -> IO ()
writeLine = putStrLn

writeToStdout :: String -> IO ()
writeToStdout = putStr

writeRawOutput :: OutputStream -> T.Text -> IO ()
writeRawOutput outputStream output = T.IO.hPutStr outputHandle output >> hFlush outputHandle
  where
    outputHandle = getOutputStreamHandle outputStream

getOutputStreamHandle :: OutputStream -> Handle
getOutputStreamHandle Stdout = stdout
getOutputStreamHandle Stderr = stderr

isInputTerminal :: IO Bool
isInputTerminal = hIsTerminalDevice stdin

isOutputTerminal :: IO Bool
isOutputTerminal = hIsTerminalDevice stdout

-- If we don't explicitly set line buffering, the output gets block-buffered
-- when stdout is not a terminal (e.g. redirected to a file or another program),
-- so messages from long-running commands don't show up until the command exits.
setLineBufferingForOutput :: IO ()
setLineBufferingForOutput = hSetBuffering stdout LineBuffering

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
