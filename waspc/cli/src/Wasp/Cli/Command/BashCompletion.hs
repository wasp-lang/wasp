module Wasp.Cli.Command.BashCompletion
  ( bashCompletion,
    printBashCompletionInstruction,
  )
where

import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import qualified System.Environment as ENV
import Wasp.Cli.Command (Command)

-- generate bash completion depending on commands input
bashCompletion :: Command ()
bashCompletion = do
  -- COMP_LINE is exposed by the bash `complete` builtin (https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
  inputEntered <- liftIO (ENV.getEnv "COMP_LINE")
  let inputWords = words inputEntered
  let inputArgs = assert (head inputArgs == "wasp") $ tail inputWords
  case inputArgs of
    [] -> listCommands commands
    ["db"] -> listCommands dbSubCommands
    [cmdPrefix] -> listMatchingCommands cmdPrefix commands
    ["db", cmdPrefix] -> listMatchingCommands cmdPrefix dbSubCommands
    _ -> liftIO . putStrLn $ ""
  where
    commands =
      [ "new",
        "new:ai",
        "version",
        "waspls",
        "completion",
        "uninstall",
        "start",
        "db",
        "clean",
        "build",
        "deploy",
        "telemetry",
        "deps",
        "dockerfile",
        "info",
        "test",
        "studio"
      ]
    dbSubCommands = ["start", "reset", "seed", "migrate-dev", "studio"]
    listMatchingCommands :: String -> [String] -> Command ()
    listMatchingCommands cmdPrefix cmdList = listCommands $ filter (cmdPrefix `isPrefixOf`) cmdList
    listCommands :: [String] -> Command ()
    listCommands cmdList = liftIO . putStrLn $ unlines cmdList

-- return the bash completion instruction
printBashCompletionInstruction :: Command ()
printBashCompletionInstruction =
  liftIO . putStrLn $
    unlines
      [ "Setting up Bash auto-completion for Wasp",
        "",
        "1. Configure your shell to use the script:",
        "   - Bash (default for most Linux and WSL):",
        "       * On macOS: edit ~/.bashrc",
        "       * On Linux: edit ~/.bash_profile",
        "   - Zsh (default for macOS): edit ~/.zshrc",
        "   - Other: check your shell's documentation on how to source a script.",
        "",
        "   Add the following line to the end of the file:",
        "     complete -o default -o nospace -C \"wasp completion:list\" wasp",
        "",
        "2. Save the file and restart your terminal.",
        "",
        "Done! Now you can use the TAB key to auto-complete Wasp commands in your shell."
      ]
