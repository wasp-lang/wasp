module Wasp.Cli.Command.ShellCompletion.CompletionArgs where

data CompletionArgs = PrintInstruction | PrintScript !Shell deriving (Show, Eq)

data Shell = Bash | Zsh | Fish deriving (Show, Eq)
