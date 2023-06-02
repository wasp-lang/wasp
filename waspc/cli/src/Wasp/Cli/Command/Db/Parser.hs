module Wasp.Cli.Command.Db.Parser (dbParser) where

import Options.Applicative
  ( Parser,
  )
import qualified Options.Applicative as O
import qualified Options.Applicative.Help as OH
import Wasp.Cli.Command.Call
  ( CommandCall (Db),
    DbArgs
      ( DbMigrateDev,
        DbReset,
        DbSeed,
        DbStart,
        DbStudio
      ),
    DbMigrateDevArgs (DbMigrateDevArgs),
  )
import Wasp.Cli.Parser.Util (mkCommand, mkCommandWithInfoModifiers)

dbParser :: Parser CommandCall
dbParser = Db <$> dbArgsParser
  where
    dbArgsParser =
      O.subparser $
        mconcat
          [ mkCommand "start" "Alias for `wasp start db`." $ pure DbStart,
            mkCommand "reset" "Drops all data and tables from development database and re-applies all migrations." $ pure DbReset,
            mkCommand
              "seed"
              "Executes a db seed function (specified via app.db.seeds). If there are multiple seeds, you can specify a seed to execute by providing its name, or if not then you will be asked to provide the name interactively."
              dbSeedArgParser,
            mkCommandWithInfoModifiers "migrate-dev" [migrateDevDescription] dbMigrateDevArgsParser,
            mkCommand "studio" "GUI for inspecting your database." $ pure DbStudio
          ]
      where
        migrateDevDescription = O.progDescDoc $ Just description
          where
            description =
              mconcat $
                OH.string
                  <$> [ "Ensures dev database corresponds to the current state of schema(entities):\n",
                        "  - Generates a new migration if there are changes in the schema.\n",
                        "  - Applies any pending migrations to the database either using the\n",
                        "    supplied migration name or asking for one."
                      ]

dbSeedArgParser :: Parser DbArgs
dbSeedArgParser = DbSeed <$> argParser
  where
    argParser = O.optional $ O.strArgument $ O.metavar "name" <> O.help "Seed name."

dbMigrateDevArgsParser :: Parser DbArgs
dbMigrateDevArgsParser = DbMigrateDev <$> argsParser
  where
    argsParser = DbMigrateDevArgs <$> nameArgParser <*> createOnlyParser
    nameArgParser = O.optional <$> O.strOption $ O.long "name" <> O.metavar "migration-name"
    createOnlyParser = O.switch $ O.long "create-only"
