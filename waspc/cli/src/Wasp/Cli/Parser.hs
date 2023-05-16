module Wasp.Cli.Parser (parserSuite) where

import Options.Applicative (Applicative (liftA2), Parser, (<**>), (<|>))
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (..))
import Wasp.Cli.Command.Deploy (parseDeploy)
import Wasp.Cli.Command.Test (parseTest)
import Wasp.Cli.Parser.Util (CommandType (CTNoIntersperse), mkCommand, mkWrapperCommand)

parserSuite :: Parser Call
parserSuite = internalCommands <|> generalCommands <|> inProjectCommands

internalCommands :: Parser Call
internalCommands = O.subparser $ mkCommand "compile" (pure Compile) "" <> O.internal

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

inProjectCommands :: Parser Call
inProjectCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        mkCommand "start" undefined "Runs Wasp app in development mode, watching for file changes.",
        mkCommand "clean" (pure Clean) "Deletes all generated code and other cached artifacts.",
        mkCommand "build" (pure Build) "Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        mkWrapperCommand "deploy" CTNoIntersperse parseDeploy "Deploys your Wasp app to cloud hosting providers.",
        mkCommand "deps" (pure Deps) "Prints the dependencies that Wasp uses in your project.",
        mkCommand "dockerfile" (pure Dockerfile) "Prints the contents of the Wasp generated Dockerfile.",
        mkCommand "info" (pure Info) "Prints basic information about current Wasp project.",
        mkCommand "test" parseTest "Executes tests in your project."
      ]