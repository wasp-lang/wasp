-- | The prompter that asks through the terminal. Only "Wasp.Cli.Console" uses
-- it, when it builds the console.
module Wasp.Cli.Console.Internal.TerminalPrompter
  ( makeTerminalPrompter,
  )
where

import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Prompter (Prompter (Prompter))
import qualified Wasp.Cli.Console.Prompter as Prompter
import Wasp.Util.Interactive (InteractiveTerminal (..), askForConfirmation, askForInput, askForRequiredInput, askToChoose)

makeTerminalPrompter :: Prompter
makeTerminalPrompter =
  Prompter
    { Prompter.askForInput = askForInput interactiveTerminal,
      Prompter.askForRequiredInput = askForRequiredInput interactiveTerminal,
      Prompter.askForConfirmation = askForConfirmation interactiveTerminal,
      Prompter.askToChoose = askToChoose interactiveTerminal
    }
  where
    interactiveTerminal =
      InteractiveTerminal
        { write = Terminal.writePrompt,
          readLine = Terminal.readLine
        }
