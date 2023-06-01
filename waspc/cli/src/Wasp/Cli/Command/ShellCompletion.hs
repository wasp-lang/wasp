module Wasp.Cli.Command.ShellCompletion
  ( completion,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative.BashCompletion as OC
import System.Environment (getExecutablePath, getProgName)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call
  ( CompletionArgs (..),
  )
import Wasp.Cli.Command.ShellCompletion.Shell (Shell (..))

completion :: CompletionArgs -> Command ()
completion PrintInstruction = printShellCompletionInstruction
completion (PrintScript shell) = liftIO $ printShellCompletionScript shell

type ProgAbsPath = FilePath

type ProgName = String

-- ProgName is our CLI executable name, and ProgAbsPath is its absolute file path.
type ShellCompletionScriptBuilder = (ProgAbsPath -> ProgName -> String)

printShellCompletionScript :: Shell -> IO ()
printShellCompletionScript shell = liftIO $ do
  -- `getExecutablePath` has some caveats:
  --   https://frasertweedale.github.io/blog-fp/posts/2022-05-10-improved-executable-path-queries.html
  -- Once we upgrade to GHC 9.4 we should change to `executablePath`, but this should be ok for
  -- our purposes.
  runningProgAbsPath <- getExecutablePath
  runningProgName <- getProgName
  putStr $ completionScriptBuilder runningProgAbsPath runningProgName
  where
    completionScriptBuilder :: ShellCompletionScriptBuilder
    completionScriptBuilder = case shell of
      Bash -> OC.bashCompletionScript
      Zsh -> OC.zshCompletionScript
      Fish -> OC.fishCompletionScript

printShellCompletionInstruction :: Command ()
printShellCompletionInstruction =
  liftIO . putStrLn $
    unlines
      [ "Run the following command to generate shell (bash, zsh, or fish) completion script for wasp on your machine:",
        "",
        "For Bash completion:",
        "",
        "    wasp completion generate bash > <your-chosen-directory>/wasp-completion",
        "",
        "After that, depending on your system, you will need to edit your bash profile:",
        "- on MacOS (OSX): you will normally want to edit ~/.bashrc",
        "- on Linux: you will normally want to edit ~/.bash_profile",
        "and add this line:",
        "",
        "     source <your-chosen-directory>/wasp-completion",
        "",
        "then reset your terminal session.",
        "",
        "-----------------------------------------------------------------------",
        "For ZSH completion:",
        "",
        "1. Create a folder for your completion scripts (if you are using Oh My ZSH, there is a folder ~/.oh-my-zsh/completions already)",
        "",
        "2.  wasp-cli completion generate zsh > <your-folder>/_wasp-completion # (the filename must begin with an underscore).",
        "",
        "3. Edit your .zshrc file to include:",
        "",
        "    fpath=(<your-folder> $fpath) # e.g. fpath=($HOME/.zsh $fpath)",
        "    autoload -U compinit && compinit",
        "",
        "then reset your terminal session.",
        "",
        "-----------------------------------------------------------------------",
        "For Fish completion:",
        "",
        "    wasp completion generate fish > <your-chosen-directory>/wasp-completion",
        "",
        "after that, you will need to edit your fish profile:",
        "- you will normally want to edit ~/.fishrc",
        "- if above doesn't work, try ~/.config/fish/config.fish",
        "and add this line:",
        "",
        "    source <your-chosen-directory>/wasp-completion",
        "",
        "then reset your terminal session."
      ]
