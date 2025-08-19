module Wasp.Cli.Command.BashCompletion
  ( bashCompletion,
    generateBashCompletionScript,
    printBashCompletionInstruction,
  )
where

import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Paths_waspc (getDataFileName)
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
        "completion:generate",
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

-- generate the bash completion script
generateBashCompletionScript :: Command ()
generateBashCompletionScript =
  liftIO $ getDataFileName "Cli/bash-completion" >>= readFile >>= putStr

-- return the bash completion instruction
printBashCompletionInstruction :: Command ()
printBashCompletionInstruction =
  liftIO . putStrLn $
    unlines
      [ "Setting up Bash auto-completion for Wasp",
        "",
        "1. Generate the completion script by running:",
        "     wasp completion:generate > <file_path>",
        "   (replace <file_path> with where you want to save the script, e.g. ~/.wasp_completion)",
        "",
        "2. Configure your shell to use the script:",
        "   - Bash:",
        "       * On macOS: edit ~/.bashrc",
        "       * On Linux: edit ~/.bash_profile",
        "   - Zsh: edit ~/.zshrc",
        "   - Other: check your shell's documentation on how to source a script.",
        "",
        "   Add the following line to the end of the file:",
        "     source <file_path>",
        "",
        "3. Save the file and restart your terminal.",
        "",
        "Done! Now you can use the TAB key to auto-complete Wasp commands in your shell."
      ]
