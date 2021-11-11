module Wasp.Cli.Command.BashCompletion
  ( bashCompletion,
    generateBashCompletionScript,
    printBashCompletionInstruction,
  )
where

import Wasp.Cli.Command (Command)
import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Paths_waspc (getDataFileName)
import qualified System.Environment as ENV

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
    commands = ["new", "version", "start", "db", "clean", "build", "telemetry", "deps"]
    dbSubCommands = ["migrate-dev", "studio"]
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
      [ "Run the following command to generate bash completion script for wasp on your machine:",
        "",
        "wasp completion:generate > <your-chosen-directory>/wasp-completion",
        "",
        "After that, depending on your system, you will need to edit your bash profile:",
        "- on MacOS (OSX): you will normally want to edit ~/.bashrc",
        "- on Linux: you will normally want to edit ~/.bash_profile",
        "and add this line:",
        "  source <your-chosen-directory>/wasp-completion",
        "then reset your terminal session."
      ]
