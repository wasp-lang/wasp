module Wasp.Cli.Command.ShellCompletion.Parser (completion) where

import Data.Maybe (fromMaybe)
import Options.Applicative (Parser)
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call
  ( Call (Completion),
    CompletionArgs (..),
    Shell (Bash, Fish, Zsh),
  )
import Wasp.Cli.Parser.Util (mkCommand)

completion :: O.Mod O.CommandFields Call
completion = mkCommand "completion" parseCompletion "Print shell generation code"

parseCompletion :: Parser Call
parseCompletion = Completion . fromMaybe ShowInstruction <$> parseShell

parseShell :: Parser (Maybe CompletionArgs)
parseShell = O.optional $ Generate <$> O.subparser (mkCommand "generate" parser "shellCodes")
  where
    parser =
      O.subparser $
        mconcat
          [ mkCommand "bash" (pure Bash) "bash script",
            mkCommand "zsh" (pure Zsh) "zsh script",
            mkCommand "fish" (pure Fish) "fish script"
          ]