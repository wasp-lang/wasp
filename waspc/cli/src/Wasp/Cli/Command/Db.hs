module Wasp.Cli.Command.Db
  ( runCommandThatRequiresDbRunning,
    parserInfo,
  )
where

import Wasp.Cli.Command (Command, runCommand)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions)
import qualified Wasp.Cli.Command.Db.Migrate as Migrate
import qualified Wasp.Cli.Command.Db.Reset as Reset
import qualified Wasp.Cli.Command.Db.Seed as Seed
import qualified Wasp.Cli.Command.Db.Studio as Studio
import Wasp.Cli.Command.Definition (CommandParserInfo, commandGroup, leaf, leafWithArgs, runWaspIO)
import Wasp.Cli.Command.Require (DbConnectionEstablished (DbConnectionEstablished), InWaspProject (InWaspProject), WaspSpecAvailable (WaspSpecAvailable), require)
import qualified Wasp.Cli.Command.Start.Db as StartDb
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter))
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))

parserInfo :: CommandParserInfo
parserInfo =
  commandGroup
    "Execute a database command. Run 'wasp db --help' for details."
    [ -- `wasp db start` is an alias for `wasp start db`.
      ("start", StartDb.parserInfo),
      ( "reset",
        leafWithArgs
          "Drop all data and tables from the dev database and re-apply all migrations."
          (runDb . Reset.reset <$> Reset.resetArgsParser)
      ),
      ( "seed",
        leafWithArgs
          "Run a database seed function (defined via app.db.seeds)."
          (runDb . Seed.seed <$> Seed.seedArgsParser)
      ),
      ( "migrate-dev",
        leafWithArgs
          ( "Ensure the dev database matches the current schema: generate a migration if the"
              <> " schema has changed and apply any pending migrations."
          )
          (runDb . Migrate.migrateDev <$> Migrate.migrateArgsParser)
      ),
      ("studio", leaf "Open a GUI for inspecting the database." (runDb Studio.studio))
    ]
  where
    -- Runs a db command (which requires a running database), reporting telemetry.
    runDb = runWaspIO . runCommandThatRequiresDbRunning

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
