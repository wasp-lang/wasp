module Wasp.Cli.Parser (parserSuite) where

import Options.Applicative (Applicative (liftA2), Parser, (<**>), (<|>))
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (..))

parserSuite :: Parser Call
parserSuite = generalCommands <|> inProjectCommands

mkCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkCommand name callCommand description =
  O.command name (O.info (O.helper <*> callCommand) (O.progDesc description))

generalCommands :: Parser Call
generalCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "GENERAL",
        mkCommand "new" undefined "Creates a new Wasp project. Run it without arguments for interactive mode.",
        mkCommand "version" (pure Version) "Prints current version of CLI.",
        mkCommand "waspls" (pure WaspLS) "Run Wasp Language Server. Add --help to get more info.",
        mkCommand "uninstall" (pure Uninstall) "Removes Wasp from your system."
      ]

deployRestArgs :: Parser String
deployRestArgs = O.strArgument (O.metavar "DEPLOY_ARGUMENTS" <> O.help "Uncovered deploy arguments")

testRestArgs :: Parser String
testRestArgs = O.strArgument (O.metavar "TEST_ARGUMENTS" <> O.help "Uncovered test arguments")

mkWrapperCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkWrapperCommand name callCommand description =
  O.command name (O.info (O.helper <*> callCommand) (O.progDesc description <> O.noIntersperse))

inProjectCommands :: Parser Call
inProjectCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        mkCommand "start" undefined "Runs Wasp app in development mode, watching for file changes.",
        mkCommand "clean" (pure Clean) "Deletes all generated code and other cached artifacts.",
        mkCommand "build" (pure Build) "Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        mkCommand "compile" (pure Compile) "--COMMAND NOT DOCUMENTED--",
        mkWrapperCommand "deploy" (Deploy <$> O.many deployRestArgs) "Deploys your Wasp app to cloud hosting providers.",
        mkCommand "deps" (pure Deps) "Prints the dependencies that Wasp uses in your project.",
        mkCommand "dockerfile" (pure Dockerfile) "Prints the contents of the Wasp generated Dockerfile.",
        mkCommand "info" (pure Info) "Prints basic information about current Wasp project.",
        mkWrapperCommand "test" (Test <$> O.many testRestArgs) "Executes tests in your project."
      ]