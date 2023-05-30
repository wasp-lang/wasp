module Wasp.Cli.Parser (parserRunnerSettings, parseCliArgs) where

import Options.Applicative (Parser, ParserInfo, ParserPrefs, (<**>), (<|>))
import qualified Options.Applicative as O
import Options.Applicative.Help (text)
import Wasp.Cli.Command.Call (CommandCall (..))
import Wasp.Cli.Command.CreateNewProject.Parser (new)
import Wasp.Cli.Command.Db.Parser (db)
import Wasp.Cli.Command.Deploy.Parser (deploy)
import Wasp.Cli.Command.ShellCompletion.Parser (completion)
import Wasp.Cli.Command.Start.Parser (start)
import Wasp.Cli.Command.Test.Parser (test)
import Wasp.Cli.Command.WaspLS.Parser (waspls)
import Wasp.Cli.Parser.Util (mkCommand, mkNormalCommand)
import qualified Wasp.Util.Terminal as Term

parseCliArgs :: IO CommandCall
parseCliArgs = uncurry O.customExecParser parserRunnerSettings

parserRunnerSettings :: (ParserPrefs, ParserInfo CommandCall)
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

parserSuite :: Parser CommandCall
parserSuite = internalCommands <|> generalCommands <|> inProjectCommands

internalCommands :: Parser CommandCall
internalCommands = O.subparser $ mkCommand "compile" [] (pure Compile) <> O.internal

generalCommands :: Parser CommandCall
generalCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "GENERAL",
        new,
        mkNormalCommand "version" "Prints current version of CLI." $ pure Version,
        waspls,
        completion,
        mkNormalCommand "uninstall" "Removes Wasp from your system." $ pure Uninstall
      ]

inProjectCommands :: Parser CommandCall
inProjectCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        start,
        db,
        mkNormalCommand "clean" "Deletes all generated code and other cached artifacts." $ pure Clean,
        mkNormalCommand "build" "Generates full web app code, ready for deployment. Use when deploying or ejecting." $ pure Build,
        deploy,
        mkNormalCommand "telemetry" "Prints telemetry status." $ pure Telemetry,
        mkNormalCommand "deps" "Prints the dependencies that Wasp uses in your project." $ pure Deps,
        mkNormalCommand "dockerfile" "Prints the contents of the Wasp generated Dockerfile." $ pure Dockerfile,
        mkNormalCommand "info" "Prints basic information about current Wasp project." $ pure Info,
        test
      ]
