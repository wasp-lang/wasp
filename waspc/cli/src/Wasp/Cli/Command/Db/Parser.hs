module Wasp.Cli.Command.Db.Parser (db) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import qualified Options.Applicative.Help as OH
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
import Wasp.Cli.Parser.Util (mkCommand, mkNormalCommand)

db :: Mod CommandFields Call
db = mkNormalCommand "db" parseDb "Executes a database command. Run 'wasp db --help' for more info."

parseDb :: Parser Call
parseDb = Db <$> parseDbArgs
  where
    customMigrateDevDescription =
      O.progDescDoc $
        Just $
          mconcat
            [ OH.string "Ensures dev database corresponds to the current state of schema(entities):\n",
              OH.string "  - Generates a new migration if there are changes in the schema.\n",
              OH.string "  - Applies any pending migrations to the database either using the\n",
              OH.string "    supplied migration name or asking for one."
            ]
    parseDbArgs :: Parser DbArgs
    parseDbArgs =
      O.subparser $
        mconcat
          [ mkNormalCommand "start" (pure DbStart) "Alias for `wasp start db`.",
            mkNormalCommand "reset" (pure DbReset) "Drops all data and tables from development database and re-applies all migrations.",
            mkNormalCommand "seed" parseDbSeedArg "Executes a db seed function (specified via app.db.seeds). If there are multiple seeds, you can specify a seed to execute by providing its name, or if not then you will be asked to provide the name interactively.",
            mkCommand "migrate-dev" [customMigrateDevDescription] parseDbMigrateDevArgs "",
            mkNormalCommand "studio" (pure DbStudio) "GUI for inspecting your database."
          ]

parseDbSeedArg :: Parser DbArgs
parseDbSeedArg = DbSeed <$> parser
  where
    parser = O.optional $ O.strArgument $ O.metavar "name" <> O.help "Seed name."

parseDbMigrateDevArgs :: Parser DbArgs
parseDbMigrateDevArgs = DbMigrateDev <$> parser
  where
    parser = DbMigrateDevArgs <$> nameArgParser <*> createOnlyParser
    nameArgParser = O.optional <$> O.strOption $ O.long "name" <> O.metavar "migration-name"
    createOnlyParser = O.switch $ O.long "create-only"
