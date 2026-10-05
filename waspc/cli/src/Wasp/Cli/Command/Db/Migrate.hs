module Wasp.Cli.Command.Db.Migrate
  ( migrateDev,
    migrateArgsParser,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..))
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
  liftIO (DbOps.migrateDevAndCopyToSource dbMigrationsDir generatedAppDir migrateArgs) >>= \case
    Left err -> throwError $ CommandError "Migrate dev failed" err
    Right () -> cliSendMessageC $ Msg.Success "Database successfully migrated."

migrateArgsParser :: Opt.Parser MigrateArgs
migrateArgsParser =
  MigrateArgs
    <$> Opt.optional (Opt.strOption (Opt.long "name" <> Opt.metavar "NAME" <> Opt.help "Migration name"))
    <*> Opt.switch (Opt.long "create-only" <> Opt.help "Create the migration without applying it")
