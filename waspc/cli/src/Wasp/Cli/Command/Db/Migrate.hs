module Wasp.Cli.Command.Db.Migrate
  ( migrateDev,
    parseMigrateArgs,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
-- Wasp generator interface.

import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Cli.Common
import qualified Wasp.Common
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)
import qualified Wasp.Generator.DbGenerator.Operations as DbOps
import qualified Wasp.Message as Msg

-- | NOTE(shayne): Performs database schema migration (based on current schema) in the generated project.
-- This assumes the wasp project migrations dir was copied from wasp source project by a previous compile.
-- The migrate function takes care of copying migrations from the generated project back to the source code.
migrateDev :: [String] -> Command ()
migrateDev optionalMigrateArgs = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let genProjectRootDir =
        waspProjectDir
          </> Cli.Common.dotWaspDirInWaspProjectDir
          </> Cli.Common.generatedCodeDirInDotWaspDir

  let waspDbMigrationsDir =
        waspProjectDir
          </> Wasp.Common.dbMigrationsDirInWaspProjectDir

  let parsedMigrateArgs = parseMigrateArgs optionalMigrateArgs
  case parsedMigrateArgs of
    Left parseError ->
      throwError $ CommandError "Migrate dev failed" parseError
    Right migrateArgs -> do
      cliSendMessageC $ Msg.Start "Performing migration..."
      migrateResult <- liftIO $ DbOps.migrateDevAndCopyToSource waspDbMigrationsDir genProjectRootDir migrateArgs
      case migrateResult of
        Left migrateError ->
          throwError $ CommandError "Migrate dev failed" migrateError
        Right () -> cliSendMessageC $ Msg.Success "Migration done."

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
