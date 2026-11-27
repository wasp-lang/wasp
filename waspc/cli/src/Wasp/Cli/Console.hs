module Wasp.Cli.Console
  ( Console (..),
    initConsole,
    isOutputShownInTerminal,
    isPromptInputEOF,
  )
where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import Wasp.Cli.Console.Capabilities (Capabilities (..), Surroundings (..), decideCapabilities)
import Wasp.Cli.Console.Internal.OutputStyle (OutputStyle (..))
import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Internal.TerminalPrompter (makeTerminalPrompter)
import Wasp.Cli.Console.Internal.TerminalReporter (makeTerminalReporter)
import Wasp.Cli.Console.Prompter (Prompter)
import Wasp.Cli.Console.Reporter (Reporter (..))
import Wasp.Util (checkIfOnCi)
import Wasp.Util.IO.Process (WriteOutput)
import qualified Wasp.Util.TextStyle as TextStyle

-- | Everything the CLI uses to talk to the user. There is a single one, created
-- at startup and handed down to the code that needs it.
data Console = Console
  { reporter :: Reporter,
    -- | Writes text out as it is: the output of the jobs, and what Wasp says
    -- that has a look of its own (e.g. the usage text).
    writeRawOutput :: WriteOutput,
    -- | Writes a command's result (e.g. the Dockerfile), which a program might
    -- read. Unlike everything else, it is written exactly as given, whatever
    -- the output is.
    writeData :: String -> IO (),
    -- | Missing when there is nobody to answer the questions. The code that
    -- wants to ask one decides what to do then.
    prompter :: Maybe Prompter
  }

initConsole :: IO Console
initConsole = do
  Terminal.setLineBufferingForOutput
  capabilities <- decideCapabilities <$> getSurroundings
  let outputStyle = if capabilities.canStyleOutput then Styled else Plain
  return
    Console
      { reporter = makeTerminalReporter outputStyle,
        writeRawOutput = makeWriteRawOutput outputStyle,
        writeData = Terminal.writeToStdout,
        prompter = if capabilities.canPrompt then Just $ makeTerminalPrompter outputStyle else Nothing
      }

-- | This is the only place that looks at where Wasp runs to decide how to talk
-- to the user.
getSurroundings :: IO Surroundings
getSurroundings = do
  isInputTerminal <- Terminal.isInputTerminal
  isOutputTerminal <- Terminal.isOutputTerminal
  isOnCi <- checkIfOnCi
  terminalType <- lookupEnv "TERM"
  noColor <- lookupEnv "NO_COLOR"
  return Surroundings {isInputTerminal, isOutputTerminal, isOnCi, terminalType, noColor}

makeWriteRawOutput :: OutputStyle -> WriteOutput
makeWriteRawOutput Styled = Terminal.writeRawOutput
makeWriteRawOutput Plain = \outputStream -> Terminal.writeRawOutput outputStream . T.pack . TextStyle.stripStyles . T.unpack

-- | Whether what Wasp writes ends up in a terminal.
isOutputShownInTerminal :: IO Bool
isOutputShownInTerminal = Terminal.isOutputTerminal

-- | True for an error caused by the user ending the input (Ctrl+D) while a
-- prompt is waiting for an answer.
isPromptInputEOF :: IOError -> Bool
isPromptInputEOF = Terminal.isInputEOF
