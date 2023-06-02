module Wasp.Cli.Parser (parserRunnerSettings, cliArgsParser) where

import Options.Applicative (Parser, ParserInfo, ParserPrefs, (<**>), (<|>))
import qualified Options.Applicative as O
import qualified Options.Applicative.Builder as OB
import qualified Options.Applicative.Help as OH
import Wasp.Cli.Command.Call (CommandCall (..))
import Wasp.Cli.Command.CreateNewProject.Parser (newParser)
import Wasp.Cli.Command.Db.Parser (dbParser)
import Wasp.Cli.Command.Deploy.Parser (deployParser)
import Wasp.Cli.Command.ShellCompletion.Parser (completionParser)
import Wasp.Cli.Command.Start.Parser (startParser)
import Wasp.Cli.Command.Test.Parser (testParser)
import Wasp.Cli.Command.WaspLS.Parser (waspLSParser)
import Wasp.Cli.Parser.Util (mkCommand, mkCommandWithInfoModifiers)
import qualified Wasp.Cli.Terminal as TermCli
import qualified Wasp.Util.Terminal as Term

cliArgsParser :: IO CommandCall
cliArgsParser = uncurry O.customExecParser parserRunnerSettings

parserRunnerSettings :: (ParserPrefs, ParserInfo CommandCall)
parserRunnerSettings = (preferences, options)
  where
    preferences = O.prefs O.showHelpOnEmpty
    options =
      O.info (topLevelCommandsParser <**> O.helper) $ O.footerDoc (Just footer)
    footer =
      OH.text $
        unlines
          [ TermCli.title "EXAMPLES",
            " wasp new MyApp",
            " wasp start",
            " wasp db migrate-dev",
            "",
            Term.applyStyles [Term.Green] "Docs:" <> " https://wasp-lang.dev/docs",
            Term.applyStyles [Term.Magenta] "Discord (chat):" <> " https://discord.gg/rzdnErX",
            Term.applyStyles [Term.Cyan] "Newsletter:" <> " https://wasp-lang.dev/#signup"
          ]

topLevelCommandsParser :: Parser CommandCall
topLevelCommandsParser =
  internalCommandsParser
    <|> generalCommandsParser
    <|> inProjectCommandsParser

internalCommandsParser :: Parser CommandCall
internalCommandsParser = O.subparser $ mkCommand "compile" [] (pure Compile) <> O.internal

generalCommandsParser :: Parser CommandCall
generalCommandsParser =
  O.subparser $
    mconcat
      [ O.commandGroup "GENERAL" <> O.hidden,
        mkCommand
          "new"
          "Creates a new Wasp project. Run it without arguments for interactive mode."
          newParser,
        mkCommand "version" "Prints current version of CLI." $ pure Version,
        mkCommand
          "waspls"
          "Run Wasp Language Server. Add --help to get more info."
          waspLSParser,
        mkCommand "completion" "Print shell completion code." completionParser,
        mkCommand "uninstall" "Removes Wasp from your system." $ pure Uninstall
      ]

inProjectCommandsParser :: Parser CommandCall
inProjectCommandsParser =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        mkCommand
          "start"
          "Runs Wasp app in development mode, watching for file changes."
          startParser,
        mkCommand "db" "Executes a database command. Run 'wasp db --help' for more info." dbParser,
        mkCommand "clean" "Deletes all generated code and other cached artifacts." $ pure Clean,
        mkCommand "build" "Generates full web app code, ready for deployment. Use when deploying or ejecting." $ pure Build,
        mkCommandWithInfoModifiers
          "deploy"
          [O.progDesc "Deploys your Wasp app to cloud hosting providers.", OB.allPositional]
          deployParser,
        mkCommand "telemetry" "Prints telemetry status." $ pure Telemetry,
        mkCommand "deps" "Prints the dependencies that Wasp uses in your project." $ pure Deps,
        mkCommand "dockerfile" "Prints the contents of the Wasp generated Dockerfile." $ pure Dockerfile,
        mkCommand "info" "Prints basic information about current Wasp project." $ pure Info,
        mkCommand "test" "Executes tests in your project." testParser
      ]
