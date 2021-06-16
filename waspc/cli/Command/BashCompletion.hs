module Command.BashCompletion
  ( bashCompletion,
    generateBashCompletionScript,
    printBashCompletionInstruction,
  )
where

import Command (Command)
import Control.Monad.IO.Class (liftIO)
import Paths_waspc (getDataFileName)

bashCompletion :: [String] -> Command ()
bashCompletion args = case args of
  [] -> listCommands
  ["db"] -> listCommandsDb
  _ -> liftIO . putStrLn $ ""

-- return available options for wasp <command>
listCommands :: Command ()
listCommands = liftIO . putStrLn $ unlines ["new", "version", "start", "db", "clean", "build", "telemetry", "deps"]

-- return available options for wasp db <command>
listCommandsDb :: Command ()
listCommandsDb = liftIO . putStrLn $ unlines ["migrate-dev", "studio"]

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
