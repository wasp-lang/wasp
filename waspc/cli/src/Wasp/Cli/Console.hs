module Wasp.Cli.Console
  ( Console (..),
    initConsole,
  )
where

import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Internal.TerminalReporter (makeTerminalReporter)
import Wasp.Cli.Console.Reporter (Reporter (..))
import Wasp.Util.IO.Process (WriteOutput)

-- | Everything the CLI uses to talk to the user. There is a single one, created
-- at startup and handed down to the code that needs it.
data Console = Console
  { reporter :: Reporter,
    -- | Where the jobs write the output that the user should see.
    writeRawOutput :: WriteOutput
  }

initConsole :: IO Console
initConsole =
  return
    Console
      { reporter = makeTerminalReporter,
        writeRawOutput = Terminal.writeJobOutput
      }
