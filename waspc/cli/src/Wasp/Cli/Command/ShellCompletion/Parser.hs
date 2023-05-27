module Wasp.Cli.Command.ShellCompletion.Parser (completion) where

import Data.Maybe (fromMaybe)
import Options.Applicative (CommandFields, Mod, Parser)
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call
  ( Call (Completion),
    CompletionArgs (..),
    Shell (Bash, Fish, Zsh),
  )
import Wasp.Cli.Parser.Util (mkNormalCommand)

completion :: Mod CommandFields Call
completion = mkNormalCommand "completion" parseCompletion "Print shell completion code."

parseCompletion :: Parser Call
parseCompletion = Completion . fromMaybe PrintInstruction <$> parseShell

parseShell :: Parser (Maybe CompletionArgs)
parseShell = O.optional $ PrintScript <$> O.subparser (mkNormalCommand "generate" parser "Generate shell completion script code of choice.")
  where
    parser =
      O.subparser $
        mconcat
          [ mkNormalCommand "bash" (pure Bash) "bash completion script.",
            mkNormalCommand "zsh" (pure Zsh) "zsh completion script.",
            mkNormalCommand "fish" (pure Fish) "fish completion script."
          ]
