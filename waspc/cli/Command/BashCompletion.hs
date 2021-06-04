module Command.BashCompletion
  ( bashCompletion,
  )
where

import Command (Command)
import Control.Monad.IO.Class (liftIO)
import qualified Paths_waspc

bashCompletion :: [String] -> Command ()
bashCompletion args = case args of
  [] -> listCommands
  ["bash-completion-generate"] -> generateBashCompletionScript
  ["bash-completion-instruction"] -> printBashCompletionInstruction
  ["db"] -> listCommandsDb
  _ -> liftIO . putStrLn $ unlines []

-- return available options for wasp <command>
listCommands :: Command ()
listCommands = liftIO . putStrLn $ unlines ["new", "version", "start", "db", "clean", "build", "telemetry", "deps"]

-- return available options for wasp db <command>
listCommandsDb :: Command ()
listCommandsDb = liftIO . putStrLn $ unlines ["migrate-dev", "studio"]

-- return the bash completion instruction
printBashCompletionInstruction :: Command ()
printBashCompletionInstruction =
  liftIO . putStrLn $
    unlines
      [ "Copy the script below to a file on your machine:",
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

generateBashCompletionScript :: Command ()
generateBashCompletionScript =
  liftIO $ do
    bashFile <- Paths_waspc.getDataFileName "Cli/bash-completion"
    contents <- readFile bashFile
    putStr contents
