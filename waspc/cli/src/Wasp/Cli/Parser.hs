module Wasp.Cli.Parser (parserRunnerSettings) where

import Options.Applicative (Parser, (<**>), (<|>))
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (..), DbArgs (..), DbMigrateDevArgs (DbMigrateDevArgs))
import Wasp.Cli.Command.CreateNewProject (parseNew)
import Wasp.Cli.Command.Deploy (parseDeploy)
import Wasp.Cli.Command.Start (parseStart)
import Wasp.Cli.Command.Test (parseTest)
import Wasp.Cli.Command.WaspLS (parseWaspLS)
import Wasp.Cli.Parser.Util (CommandType (CTNoIntersperse), mkCommand, mkWrapperCommand)
import qualified Wasp.Util.Terminal as Term

parserRunnerSettings :: (O.ParserPrefs, O.ParserInfo Call)
parserRunnerSettings = (preferences, options)
  where
    options =
      O.info
        (parserSuite <**> O.helper)
        ( O.fullDesc
            <> O.footer
              -- FIXME: Fix stdout formatting.
              ( unlines
                  [ Term.applyStyles [Term.Green] "Docs:" <> " https://wasp-lang.dev/docs",
                    Term.applyStyles [Term.Magenta] "Discord (chat):" <> " https://discord.gg/rzdnErX",
                    Term.applyStyles [Term.Cyan] "Newsletter:" <> " https://wasp-lang.dev/#signup"
                  ]
              )
        )
    preferences = O.prefs O.showHelpOnEmpty

parserSuite :: Parser Call
parserSuite = internalCommands <|> generalCommands <|> inProjectCommands

internalCommands :: Parser Call
internalCommands = O.subparser $ mkCommand "compile" (pure Compile) "" <> O.internal

generalCommands :: Parser Call
generalCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "GENERAL",
        mkCommand "new" parseNew "Creates a new Wasp project. Run it without arguments for interactive mode.",
        mkCommand "version" (pure Version) "Prints current version of CLI.",
        mkCommand "waspls" parseWaspLS "Run Wasp Language Server. Add --help to get more info.",
        mkCommand "uninstall" (pure Uninstall) "Removes Wasp from your system."
      ]

parseDbSeedArg :: Parser DbArgs
parseDbSeedArg = DbSeed <$> parser
  where
    parser = O.optional $ O.strArgument $ O.metavar "name" <> O.help "Seed name."

-- FIXME: Currently we using primitive types. Let's see if we can refine with concrete sum types.
parseDbMigrateDevArgs :: Parser DbArgs
parseDbMigrateDevArgs = DbMigrateDev <$> parser
  where
    parser = DbMigrateDevArgs <$> nameArgParser <*> createOnlyParser
    nameArgParser = O.optional <$> O.strOption $ O.long "name" <> O.metavar "migration-name"
    createOnlyParser = O.switch $ O.long "create-only"

parseDbArgs :: Parser DbArgs
parseDbArgs =
  O.subparser $
    mconcat
      [ mkCommand "start" (pure DbStart) "Alias for `wasp start db`.",
        mkCommand "reset" (pure DbReset) "Drops all data and tables from development database and re-applies all migrations.",
        -- FIXME: Fix stdout formatting.
        mkCommand "seed" parseDbSeedArg "Executes a db seed function (specified via app.db.seeds). If there are multiple seeds, you can specify a seed to execute by providing its name, or if not then you will be asked to provide the name interactively.",
        mkCommand "migrate-dev" parseDbMigrateDevArgs "Ensures dev database corresponds to the current state of schema(entities):\n  - Generates a new migration if there are changes in the schema.\n  - Applies any pending migrations to the database either using the\n    supplied migration name or asking for one.",
        mkCommand "studio" (pure DbStudio) "GUI for inspecting your database."
      ]

parseDb :: Parser Call
parseDb = Db <$> parseDbArgs

inProjectCommands :: Parser Call
inProjectCommands =
  O.subparser $
    mconcat
      [ O.commandGroup "IN PROJECT",
        mkCommand "start" parseStart "Runs Wasp app in development mode, watching for file changes.",
        mkCommand "db" parseDb "Executes a database command. Run 'wasp db --help' for more info.",
        mkCommand "clean" (pure Clean) "Deletes all generated code and other cached artifacts.",
        mkCommand "build" (pure Build) "Generates full web app code, ready for deployment. Use when deploying or ejecting.",
        mkWrapperCommand "deploy" CTNoIntersperse parseDeploy "Deploys your Wasp app to cloud hosting providers.",
        mkCommand "telemetry" (pure Telemetry) "Prints telemetry status.",
        mkCommand "deps" (pure Deps) "Prints the dependencies that Wasp uses in your project.",
        mkCommand "dockerfile" (pure Dockerfile) "Prints the contents of the Wasp generated Dockerfile.",
        mkCommand "info" (pure Info) "Prints basic information about current Wasp project.",
        mkCommand "test" parseTest "Executes tests in your project."
      ]
