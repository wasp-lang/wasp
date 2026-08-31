module Wasp.Cli.Command.Show
  ( showCommand,
    subcommands,
  )
where

import Data.List (intercalate)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Show.Build (buildShowSubcommand)
import Wasp.Cli.Command.Show.Spec (specShowSubcommand)
import Wasp.Cli.Command.Show.Subcommand (ShowSubcommand (..), runShowSubcommand)
import Wasp.Cli.Util.Parser (withArguments)

-- | Prints information about the project, e.g. its current build with
-- `wasp show build`.
showCommand :: Arguments -> Command ()
showCommand = withArguments "wasp show" showParser id

subcommands :: [ShowSubcommand]
subcommands = [specShowSubcommand, buildShowSubcommand]

showParser :: Opt.Parser (Command ())
showParser =
  Opt.hsubparser $ mconcat $ subcommandsMetavar : (toOptCommand <$> subcommands)
  where
    toOptCommand subcommand =
      Opt.command subcommand.name $
        Opt.info (runShowSubcommand subcommand <$> jsonFlagParser) $
          Opt.progDesc subcommand.description

    jsonFlagParser =
      Opt.switch (Opt.long "json" <> Opt.help "Render output as JSON")

    subcommandsMetavar =
      Opt.metavar $ "<" <> intercalate "|" ((.name) <$> subcommands) <> ">"
