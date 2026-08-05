module Wasp.Cli.Command.Show.ArgumentsParser
  ( ShowArgs (..),
    ShowSubcommandArgs (..),
    showArgsParser,
  )
where

import qualified Options.Applicative as Opt

data ShowArgs
  = ShowSpec ShowSubcommandArgs
  | ShowBuild ShowSubcommandArgs

newtype ShowSubcommandArgs = ShowSubcommandArgs
  { json :: Bool
  }

showArgsParser :: Opt.Parser ShowArgs
showArgsParser =
  Opt.hsubparser $
    mconcat
      [ Opt.metavar "<spec|build>",
        Opt.command "spec" $
          Opt.info (ShowSpec <$> subcommandArgsParser specJsonHelp) $
            Opt.progDesc "Prints an overview of your app: routes, pages, queries, actions, and more",
        Opt.command "build" $
          Opt.info (ShowBuild <$> subcommandArgsParser buildJsonHelp) $
            Opt.progDesc "Prints information about your app's current build"
      ]
  where
    specJsonHelp =
      "Print the full evaluated app spec as JSON. The schema follows Wasp's "
        <> "internal spec format and may change between Wasp versions."
    buildJsonHelp = "Print the build information as JSON."

subcommandArgsParser :: String -> Opt.Parser ShowSubcommandArgs
subcommandArgsParser jsonHelp =
  ShowSubcommandArgs
    <$> Opt.switch (Opt.long "json" <> Opt.help jsonHelp)
