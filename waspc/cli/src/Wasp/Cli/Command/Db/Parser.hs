module Wasp.Cli.Command.Db.Parser (db) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    help,
    long,
    metavar,
    optional,
    strArgument,
    strOption,
    subparser,
    switch,
  )
import Wasp.Cli.Command.Call
  ( Call (Db),
    DbArgs
      ( DbMigrateDev,
        DbReset,
        DbSeed,
        DbStart,
        DbStudio
      ),
    DbMigrateDevArgs (DbMigrateDevArgs),
  )
import Wasp.Cli.Parser.Util (mkCommand)

db :: Mod CommandFields Call
db = mkCommand "db" parseDb "Executes a database command. Run 'wasp db --help' for more info."

parseDb :: Parser Call
parseDb = Db <$> parseDbArgs
  where
    parseDbArgs :: Parser DbArgs
    parseDbArgs =
      subparser $
        mconcat
          [ mkCommand "start" (pure DbStart) "Alias for `wasp start db`.",
            mkCommand "reset" (pure DbReset) "Drops all data and tables from development database and re-applies all migrations.",
            -- FIXME: Fix stdout formatting.
            mkCommand "seed" parseDbSeedArg "Executes a db seed function (specified via app.db.seeds). If there are multiple seeds, you can specify a seed to execute by providing its name, or if not then you will be asked to provide the name interactively.",
            mkCommand "migrate-dev" parseDbMigrateDevArgs "Ensures dev database corresponds to the current state of schema(entities):\n  - Generates a new migration if there are changes in the schema.\n  - Applies any pending migrations to the database either using the\n    supplied migration name or asking for one.",
            mkCommand "studio" (pure DbStudio) "GUI for inspecting your database."
          ]

parseDbSeedArg :: Parser DbArgs
parseDbSeedArg = DbSeed <$> parser
  where
    parser = optional $ strArgument $ metavar "name" <> help "Seed name."

parseDbMigrateDevArgs :: Parser DbArgs
parseDbMigrateDevArgs = DbMigrateDev <$> parser
  where
    parser = DbMigrateDevArgs <$> nameArgParser <*> createOnlyParser
    nameArgParser = optional <$> strOption $ long "name" <> metavar "migration-name"
    createOnlyParser = switch $ long "create-only"
