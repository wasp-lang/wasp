module Wasp.Cli.Command.Db.Migrate
  ( migrateDev,
    migrateArgsParser,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)
import qualified Wasp.Generator.DbGenerator.Operations as DbOps
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)
import Wasp.Project.Db.Migrations (DbMigrationsDir, dbMigrationsDirInWaspProjectDir)

-- | NOTE(shayne): Performs database schema migration (based on current schema) in the generated project.
-- This assumes the wasp project migrations dir was copied from wasp source project by a previous compile.
-- The migrate function takes care of copying migrations from the generated project back to the source code.
migrateDev :: MigrateArgs -> Command ()
migrateDev migrateArgs = do
  InWaspProject waspProjectDir <- require
  let waspDbMigrationsDir = waspProjectDir </> dbMigrationsDirInWaspProjectDir
  let generatedAppDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir

  migrateDatabase migrateArgs generatedAppDir waspDbMigrationsDir

migrateDatabase :: MigrateArgs -> Path' Abs (Dir GeneratedAppDir) -> Path' Abs (Dir DbMigrationsDir) -> Command ()
migrateDatabase migrateArgs generatedAppDir dbMigrationsDir = do
  cliSendMessageC $ Msg.Start "Starting database migration..."
  liftIO tryMigrate >>= \case
    Left err -> throwError $ CommandError "Migrate dev failed" err
    Right () -> cliSendMessageC $ Msg.Success "Database successfully migrated."
  where
    tryMigrate = runExceptT $ ExceptT $ DbOps.migrateDevAndCopyToSource dbMigrationsDir generatedAppDir migrateArgs

migrateArgsParser :: Opt.Parser MigrateArgs
migrateArgsParser =
  (\name createOnly -> defaultMigrateArgs {_migrationName = name, _isCreateOnlyMigration = createOnly})
    <$> Opt.optional
      ( Opt.strOption $
          Opt.long "name"
            <> Opt.metavar "MIGRATION_NAME"
            <> Opt.help "Name to use for the generated migration"
      )
    <*> Opt.switch
      ( Opt.long "create-only"
          <> Opt.help "Only create the migration file without applying it to the database"
      )
