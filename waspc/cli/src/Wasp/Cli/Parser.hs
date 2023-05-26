module Wasp.Cli.Parser (parserRunnerSettings, parseCliArgs) where

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
import Wasp.Cli.Parser.Util (mkNormalCommand)
import qualified Wasp.Util.Terminal as Term

parseCliArgs :: IO Call
parseCliArgs = uncurry O.customExecParser parserRunnerSettings

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
internalCommands = O.subparser $ mkNormalCommand "compile" (pure Compile) "" <> O.internal

generalCommands :: Parser Call
generalCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "GENERAL",
        new,
        mkNormalCommand "version" (pure Version) "Prints current version of CLI.",
        waspls,
        completion,
        mkNormalCommand "uninstall" (pure Uninstall) "Removes Wasp from your system."
      ]

inProjectCommands :: Parser Call
inProjectCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        start,
        db,
        mkNormalCommand "clean" (pure Clean) "Deletes all generated code and other cached artifacts.",
        mkNormalCommand "build" (pure Build) "Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        deploy,
        mkNormalCommand "telemetry" (pure Telemetry) "Prints telemetry status.",
        mkNormalCommand "deps" (pure Deps) "Prints the dependencies that Wasp uses in your project.",
        mkNormalCommand "dockerfile" (pure Dockerfile) "Prints the contents of the Wasp generated Dockerfile.",
        mkNormalCommand "info" (pure Info) "Prints basic information about current Wasp project.",
        test
      ]
