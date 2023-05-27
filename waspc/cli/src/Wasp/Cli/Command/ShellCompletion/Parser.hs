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
completion = mkNormalCommand "completion" "Print shell completion code." parseCompletion

parseCompletion :: Parser Call
parseCompletion = Completion . fromMaybe PrintInstruction <$> parseShell

parseShell :: Parser (Maybe CompletionArgs)
parseShell =
  O.optional $
    PrintScript
      <$> O.subparser
        (mkNormalCommand "generate" "Generate shell completion script code of choice." parser)
  where
    parser =
      O.subparser $
        mconcat
          [ mkNormalCommand "bash" "bash completion script." $ pure Bash,
            mkNormalCommand "zsh" "zsh completion script." $ pure Zsh,
            mkNormalCommand "fish" "fish completion script." $ pure Fish
          ]
