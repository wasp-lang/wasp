module Wasp.Cli.Command.ShellCompletion.Parser (completion) where

import Data.Maybe (fromMaybe)
import Options.Applicative (CommandFields, Mod, Parser)
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call
  ( Call (Completion),
    CompletionArgs (..),
    Shell (Bash, Fish, Zsh),
  )
import Wasp.Cli.Parser.Util (mkCommand)

completion :: Mod CommandFields Call
completion = mkCommand "completion" parseCompletion "Print shell completion code."

parseCompletion :: Parser Call
parseCompletion = Completion . fromMaybe ShowInstruction <$> parseShell

parseShell :: Parser (Maybe CompletionArgs)
parseShell = O.optional $ Generate <$> O.subparser (mkCommand "generate" parser "Generate shell completion script code of choice.")
  where
    parser =
      O.subparser $
        mconcat
          [ mkCommand "bash" (pure Bash) "bash completion script.",
            mkCommand "zsh" (pure Zsh) "zsh completion script.",
            mkCommand "fish" (pure Fish) "fish completion script."
          ]
