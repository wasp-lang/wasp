module Wasp.Cli.Parser (parserRunnerSettings) where

import Options.Applicative (Parser, ParserInfo, ParserPrefs, (<**>), (<|>))
import qualified Options.Applicative as O
import Options.Applicative.Help (text)
import Wasp.Cli.Command.Call (Call (..))
import Wasp.Cli.Command.CreateNewProject.Parser (new)
import Wasp.Cli.Command.Db.Parser (db)
import Wasp.Cli.Command.Deploy.Parser (deploy)
import Wasp.Cli.Command.ShellCompletion.Parser (completion)
import Wasp.Cli.Command.Start.Parser (start)
import Wasp.Cli.Command.Test.Parser (test)
import Wasp.Cli.Command.WaspLS.Parser (waspls)
import Wasp.Cli.Parser.Util (mkCommand)
import qualified Wasp.Util.Terminal as Term

parserRunnerSettings :: (ParserPrefs, ParserInfo Call)
parserRunnerSettings = (preferences, options)
  where
    preferences = O.prefs O.showHelpOnEmpty
    options =
      O.info (parserSuite <**> O.helper) $ O.footerDoc (Just footer)
    footer =
      text $
        unlines
          [ Term.applyStyles [Term.Green] "Docs:" <> " https://wasp-lang.dev/docs",
            Term.applyStyles [Term.Magenta] "Discord (chat):" <> " https://discord.gg/rzdnErX",
            Term.applyStyles [Term.Cyan] "Newsletter:" <> " https://wasp-lang.dev/#signup"
          ]

parserSuite :: Parser Call
parserSuite = internalCommands <|> generalCommands <|> inProjectCommands

internalCommands :: Parser Call
internalCommands = O.subparser $ mkCommand "compile" (pure Compile) "" <> O.internal

generalCommands :: Parser Call
generalCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "GENERAL",
        new,
        mkCommand "version" (pure Version) "Prints current version of CLI.",
        waspls,
        completion,
        mkCommand "uninstall" (pure Uninstall) "Removes Wasp from your system."
      ]

inProjectCommands :: Parser Call
inProjectCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        start,
        db,
        mkCommand "clean" (pure Clean) "Deletes all generated code and other cached artifacts.",
        mkCommand "build" (pure Build) "Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        deploy,
        mkCommand "telemetry" (pure Telemetry) "Prints telemetry status.",
        mkCommand "deps" (pure Deps) "Prints the dependencies that Wasp uses in your project.",
        mkCommand "dockerfile" (pure Dockerfile) "Prints the contents of the Wasp generated Dockerfile.",
        mkCommand "info" (pure Info) "Prints basic information about current Wasp project.",
        test
      ]
