module Wasp.Cli.Command.Db
  ( runCommandThatRequiresDbRunning,
    parserInfo,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Command (Command, runCommand)
import qualified Wasp.Cli.Command.Call as Call
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions)
import qualified Wasp.Cli.Command.Db.Migrate as Migrate
import qualified Wasp.Cli.Command.Db.Reset as Reset
import qualified Wasp.Cli.Command.Db.Seed as Seed
import qualified Wasp.Cli.Command.Db.Studio as Studio
import Wasp.Cli.Command.Require (DbConnectionEstablished (DbConnectionEstablished), InWaspProject (InWaspProject), WaspSpecAvailable (WaspSpecAvailable), require)
import qualified Wasp.Cli.Command.Start.Db as StartDb
import Wasp.Cli.Command.Telemetry (runWithTelemetry)
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter))
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))

parserInfo :: Opt.ParserInfo (IO ())
parserInfo =
  Opt.info
    dbSubcommands
    (Opt.progDesc "Execute a database command. Run 'wasp db --help' for details.")
  where
    dbSubcommands =
      Opt.hsubparser $
        mconcat
          [ -- `wasp db start` is an alias for `wasp start db`.
            Opt.command "start" StartDb.parserInfo,
            Opt.command "reset" resetInfo,
            Opt.command "seed" seedInfo,
            Opt.command "migrate-dev" migrateDevInfo,
            Opt.command "studio" studioInfo
          ]
    resetInfo =
      Opt.info
        (runDbCommand . Reset.reset <$> Reset.resetArgsParser)
        (Opt.progDesc "Drop all data and tables from the dev database and re-apply all migrations.")
    seedInfo =
      Opt.info
        (runDbCommand . Seed.seed <$> Seed.seedArgsParser)
        (Opt.progDesc "Run a database seed function (defined via app.db.seeds).")
    migrateDevInfo =
      Opt.info
        (runDbCommand . Migrate.migrateDev <$> Migrate.migrateArgsParser)
        ( Opt.progDesc
            ( "Ensure the dev database matches the current schema: generate a migration if the"
                <> " schema has changed and apply any pending migrations."
            )
        )
    studioInfo =
      Opt.info
        (pure $ runDbCommand Studio.studio)
        (Opt.progDesc "Open a GUI for inspecting the database.")

-- | Runs a db command (that requires a running database), reporting telemetry.
runDbCommand :: Command () -> IO ()
runDbCommand = runWithTelemetry Call.Other . runCommandThatRequiresDbRunning

runCommandThatRequiresDbRunning :: Command a -> IO ()
runCommandThatRequiresDbRunning = runCommand . makeDbCommand

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = do
  -- Ensure code is generated and npm dependencies are installed.
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  _ <- compileWithOptions $ compileOptions waspProjectDir
  DbConnectionEstablished <- require
  cmd
  where
    compileOptions waspProjectDir =
      (defaultCompileOptions waspProjectDir)
        { -- Ignore "DB needs migration warnings" during database commands, as that is redundant
          -- for `db migrate-dev` and not helpful for `db studio`.
          generatorWarningsFilter =
            filter
              ( \case
                  GeneratorNeedsMigrationWarning _ -> False
                  _ -> True
              )
        }
