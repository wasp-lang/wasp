module Wasp.Cli.Command.Db.Migrate
  ( migrateDev,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Call (DbMigrateDevArgs (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..))
import qualified Wasp.Generator.DbGenerator.Operations as DbOps
import qualified Wasp.Message as Msg
import Wasp.Project.Db.Migrations (DbMigrationsDir, dbMigrationsDirInWaspProjectDir)

-- | NOTE(shayne): Performs database schema migration (based on current schema) in the generated project.
-- This assumes the wasp project migrations dir was copied from wasp source project by a previous compile.
-- The migrate function takes care of copying migrations from the generated project back to the source code.
migrateDev :: DbMigrateDevArgs -> Command ()
migrateDev optionalMigrateArgs = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let waspDbMigrationsDir = waspProjectDir </> dbMigrationsDirInWaspProjectDir
  let projectRootDir =
        waspProjectDir
          </> Cli.Common.dotWaspDirInWaspProjectDir
          </> Cli.Common.generatedCodeDirInDotWaspDir

  migrateDatabase optionalMigrateArgs projectRootDir waspDbMigrationsDir

migrateDatabase :: DbMigrateDevArgs -> Path' Abs (Dir ProjectRootDir) -> Path' Abs (Dir DbMigrationsDir) -> Command ()
migrateDatabase optionalMigrateArgs projectRootDir dbMigrationsDir = do
  cliSendMessageC $ Msg.Start "Starting database migration..."
  liftIO tryMigrate >>= \case
    Left err -> throwError $ CommandError "Migrate dev failed" err
    Right () -> cliSendMessageC $ Msg.Success "Database successfully migrated."
  where
    tryMigrate =
      DbOps.migrateDevAndCopyToSource dbMigrationsDir projectRootDir $
        migrateArgs optionalMigrateArgs
      where
        migrateArgs DbMigrateDevArgs {dbMigrateName = name, dbMigrateCreateOnly = isCreateOnly} =
          MigrateArgs {_isCreateOnlyMigration = isCreateOnly, _migrationName = name}
