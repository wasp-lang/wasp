module Wasp.Cli.Console
  ( Console (..),
    initConsole,
    isOutputShownInTerminal,
    isPromptInputEOF,
  )
where

import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Internal.TerminalPrompter (makeTerminalPrompter)
import Wasp.Cli.Console.Internal.TerminalReporter (makeTerminalReporter)
import Wasp.Cli.Console.Prompter (Prompter)
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Job as Job
import Wasp.Util.IO.Process (OutputStream (..), WriteOutput)

-- | Everything the CLI uses to talk to the user. There is a single one, created
-- at startup and handed down to the code that needs it.
data Console = Console
  { reporter :: Reporter,
    -- | Writes text out as it is: the output of the jobs, and what Wasp says
    -- that has a look of its own (e.g. the usage text).
    writeRawOutput :: WriteOutput,
    -- | Missing when there is nobody to answer the questions. The code that
    -- wants to ask one decides what to do then.
    prompter :: Maybe Prompter
  }

initConsole :: IO Console
initConsole = do
  Terminal.setLineBufferingForOutput
  canPrompt <- Terminal.isInputTerminal
  return
    Console
      { reporter = makeTerminalReporter,
        writeRawOutput = Terminal.writeRawOutput,
        prompter = if canPrompt then Just makeTerminalPrompter else Nothing
      }

-- | Whether what Wasp writes ends up in a terminal.
isOutputShownInTerminal :: IO Bool
isOutputShownInTerminal = Terminal.isOutputTerminal

-- | True for an error caused by the user ending the input (Ctrl+D) while a
-- prompt is waiting for an answer.
isPromptInputEOF :: IOError -> Bool
isPromptInputEOF = Terminal.isInputEOF
