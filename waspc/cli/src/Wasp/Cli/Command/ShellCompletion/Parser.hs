module Wasp.Cli.Command.ShellCompletion.Parser (completionParser) where

import Data.Maybe (fromMaybe)
import Options.Applicative (Parser)
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call
  ( CommandCall (Completion),
  )
import Wasp.Cli.Command.ShellCompletion.CompletionArgs (CompletionArgs (..), Shell (..))
import Wasp.Cli.Parser.Util (mkCommand)

completionParser :: Parser CommandCall
completionParser = Completion . fromMaybe PrintInstruction <$> printScriptParser

printScriptParser :: Parser (Maybe CompletionArgs)
printScriptParser =
  O.optional $
    PrintScript
      <$> O.subparser
        (mkCommand "generate" "Generate shell completion script code of choice." shellParser)
  where
    shellParser =
      O.subparser $
        mconcat
          [ mkCommand "bash" "bash completion script." $ pure Bash,
            mkCommand "zsh" "zsh completion script." $ pure Zsh,
            mkCommand "fish" "fish completion script." $ pure Fish
          ]
