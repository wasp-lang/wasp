module Wasp.Cli.Command.Db.Migrate
  ( migrateDev,
    parseMigrateArgs,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)
import qualified Wasp.Generator.DbGenerator.Operations as DbOps
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Project.Db.Migrations (DbMigrationsDir, dbMigrationsDirInWaspProjectDir)

-- | NOTE(shayne): Performs database schema migration (based on current schema) in the generated project.
-- This assumes the wasp project migrations dir was copied from wasp source project by a previous compile.
-- The migrate function takes care of copying migrations from the generated project back to the source code.
migrateDev :: [String] -> Command ()
migrateDev optionalMigrateArgs = do
  InWaspProject waspProjectDir <- require
  let waspDbMigrationsDir = waspProjectDir </> dbMigrationsDirInWaspProjectDir
  let projectRootDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir

  migrateDatabase optionalMigrateArgs projectRootDir waspDbMigrationsDir

migrateDatabase :: [String] -> Path' Abs (Dir ProjectRootDir) -> Path' Abs (Dir DbMigrationsDir) -> Command ()
migrateDatabase optionalMigrateArgs projectRootDir dbMigrationsDir = do
  cliSendMessageC $ Msg.Start "Starting database migration..."
  liftIO tryMigrate >>= \case
    Left err -> throwError $ CommandError "Migrate dev failed" err
    Right () -> cliSendMessageC $ Msg.Success "Database successfully migrated."
  where
    tryMigrate = runExceptT $ do
      migrateArgs <- liftEither $ parseMigrateArgs optionalMigrateArgs
      ExceptT $ DbOps.migrateDevAndCopyToSource dbMigrationsDir projectRootDir migrateArgs

-- | Basic parsing of db-migrate args. In the future, we could use a smarter parser
-- for this (and all other CLI arg parsing).
parseMigrateArgs :: [String] -> Either String MigrateArgs
parseMigrateArgs migrateArgs = do
  go migrateArgs defaultMigrateArgs
  where
    go :: [String] -> MigrateArgs -> Either String MigrateArgs
    go [] mArgs = Right mArgs
    go ("--create-only" : rest) mArgs = go rest $ mArgs {_isCreateOnlyMigration = True}
    go ("--name" : name : rest) mArgs = go rest $ mArgs {_migrationName = Just name}
    go unknown _ = Left $ "Unknown migrate arg(s): " ++ unwords unknown
