module Wasp.Cli.Command.ShellCompletion
  ( completion,
  )
where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getProgName)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call
  ( CompletionArgs (..),
    Shell (Bash, Fish, Zsh),
  )

completion :: CompletionArgs -> Command ()
completion args = case args of
  ShowInstruction -> printShellCompletionInstruction
  Generate shell -> case shell of
    Bash -> generateBash
    Zsh -> generateZsh
    Fish -> generateFish

-- FIXME: Update instructions.
printShellCompletionInstruction :: Command ()
printShellCompletionInstruction =
  liftIO . putStrLn $
    unlines
      [ "Run the following command to generate shell (bash, zsh, or fish) completion script for wasp on your machine:",
        "",
        "wasp completion:generate:bash > <your-chosen-directory>/wasp-completion",
        "",
        "After that, depending on your system, you will need to edit your bash profile:",
        "- on MacOS (OSX): you will normally want to edit ~/.bashrc",
        "- on Linux: you will normally want to edit ~/.bash_profile",
        "and add this line:",
        "  source <your-chosen-directory>/wasp-completion",
        "then reset your terminal session."
      ]

generateSh :: (String -> String -> String) -> Command ()
generateSh sh = liftIO $ do
  name <- getProgName
  putStr $ sh name name

generateBash :: Command ()
generateBash = generateSh bashCompletionScript

generateZsh :: Command ()
generateZsh = generateSh zshCompletionScript

generateFish :: Command ()
generateFish = generateSh fishCompletionScript

-- TODO: Use newly exposed`*shCompletionScript` functions from optparse-applicative >0.17
-- once it is released instead of using these text functions manually.
bashCompletionScript :: String -> String -> String
bashCompletionScript prog progn =
  unlines
    [ "_" ++ progn ++ "()",
      "{",
      "    local CMDLINE",
      "    local IFS=$'\\n'",
      "    CMDLINE=(--bash-completion-index $COMP_CWORD)",
      "",
      "    for arg in ${COMP_WORDS[@]}; do",
      "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)",
      "    done",
      "",
      "    COMPREPLY=( $(" ++ prog ++ " \"${CMDLINE[@]}\") )",
      "}",
      "",
      "complete -o filenames -F _" ++ progn ++ " " ++ progn
    ]

fishCompletionScript :: String -> String -> String
fishCompletionScript prog progn =
  unlines
    [ " function _" ++ progn,
      "    set -l cl (commandline --tokenize --current-process)",
      "    # Hack around fish issue #3934",
      "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)",
      "    set -l cn (count $cn)",
      "    set -l tmpline --bash-completion-enriched --bash-completion-index $cn",
      "    for arg in $cl",
      "      set tmpline $tmpline --bash-completion-word $arg",
      "    end",
      "    for opt in (" ++ prog ++ " $tmpline)",
      "      if test -d $opt",
      "        echo -E \"$opt/\"",
      "      else",
      "        echo -E \"$opt\"",
      "      end",
      "    end",
      "end",
      "",
      "complete --no-files --command " ++ progn ++ " --arguments '(_" ++ progn ++ ")'"
    ]

zshCompletionScript :: String -> String -> String
zshCompletionScript prog progn =
  unlines
    [ "#compdef " ++ progn,
      "",
      "local request",
      "local completions",
      "local word",
      "local index=$((CURRENT - 1))",
      "",
      "request=(--bash-completion-enriched --bash-completion-index $index)",
      "for arg in ${words[@]}; do",
      "  request=(${request[@]} --bash-completion-word $arg)",
      "done",
      "",
      "IFS=$'\\n' completions=($( " ++ prog ++ " \"${request[@]}\" ))",
      "",
      "for word in $completions; do",
      "  local -a parts",
      "",
      "  # Split the line at a tab if there is one.",
      "  IFS=$'\\t' parts=($( echo $word ))",
      "",
      "  if [[ -n $parts[2] ]]; then",
      "     if [[ $word[1] == \"-\" ]]; then",
      "       local desc=(\"$parts[1] ($parts[2])\")",
      "       compadd -d desc -- $parts[1]",
      "     else",
      "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))",
      "       compadd -l -d desc -- $parts[1]",
      "     fi",
      "  else",
      "    compadd -f -- $word",
      "  fi",
      "done"
    ]
