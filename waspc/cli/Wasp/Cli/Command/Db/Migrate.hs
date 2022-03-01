module Wasp.Cli.Command.Db.Migrate
  ( migrateDev,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
-- Wasp generator interface.

import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Cli.Terminal (asWaspFailureMessage, asWaspStartMessage, asWaspSuccessMessage)
import qualified Wasp.Common
import qualified Wasp.Generator.DbGenerator.Operations as DbOps

-- | NOTE(shayne): Performs database schema migration (based on current schema) in the generated project.
-- This assumes the wasp project migrations dir was copied from wasp source project by a previous compile.
-- The migrate function takes care of copying migrations from the generated project back to the source code.
migrateDev :: Maybe String -> Command ()
migrateDev maybeMigrationName = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let genProjectRootDir =
        waspProjectDir
          </> Cli.Common.dotWaspDirInWaspProjectDir
          </> Cli.Common.generatedCodeDirInDotWaspDir

  let waspDbMigrationsDir =
        waspProjectDir
          </> Wasp.Common.dbMigrationsDirInWaspProjectDir

  waspSaysC $ asWaspStartMessage "Performing migration..."
  migrateResult <- liftIO $ DbOps.migrateDevAndCopyToSource waspDbMigrationsDir genProjectRootDir maybeMigrationName
  case migrateResult of
    Left migrateError ->
      throwError $ CommandError $ asWaspFailureMessage "Migrate dev failed:" ++ migrateError
    Right () -> waspSaysC $ asWaspSuccessMessage "Migration done."
