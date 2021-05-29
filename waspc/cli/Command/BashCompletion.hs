module Command.BashCompletion
  ( bashCompletion,
  )
where

import Command (Command)
import Control.Monad.IO.Class (liftIO)


bashCompletion :: [String] -> Command ()
bashCompletion args = case args of
  [] -> listCommands
  ["completion-script"] -> printCompletionScript
  ["db"] -> listCommandsDb
  _ -> printNoSuggestion

-- return available options for wasp <command>
listCommands :: Command ()
listCommands = liftIO . putStrLn $ unlines ["new", "version", "start", "db", "clean", "build", "telemetry", "deps"]

-- return available options for wasp db <command>
listCommandsDb :: Command ()
listCommandsDb = liftIO . putStrLn $ unlines ["migrate-dev",  "studio"]

-- return the bash completion script with instruction
printCompletionScript :: Command ()
printCompletionScript =
  liftIO . putStrLn $
    unlines
      [
        "Copying the script below to a file:",
        "",
        "_wasp() {",
        "  local cur prev",
        "  cur=\"${COMP_WORDS[COMP_CWORD]}\"",
        "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"",
        "  case ${COMP_CWORD} in",
        "   1)",
        "     _wasp_commands=$( wasp commands )",
        "     COMPREPLY=( $(compgen -W \"${_wasp_commands}\" -- \"${cur}\") )",
        "     ;;",
        "   2)",
        "     _wasp_second_commands=$( wasp commands ${prev} )",
        "     COMPREPLY=( $(compgen -W \"${_wasp_second_commands}\" -- \"${cur}\") )",
        "     ;;",
        "   *)",
        "     COMPREPLY=()",
        "  esac",
        "}",
        "complete -o default -o nospace -F _wasp wasp",
        "",
        "After that, depending on your system, you will need open your bash profile:",
        "- on MacOS (OSX): it usually locates at ~/.bashrc",
        "- on Linux: it usually locates at ~/.bash_profile",
        "and add this line:",
        "  source <full-file-path-to-the-downloaded-wasp-completion-script",
        "then reset your terminal."
      ]

-- return an empty list if the command is unknown
printNoSuggestion :: Command ()
printNoSuggestion = liftIO . putStrLn $ unlines []
