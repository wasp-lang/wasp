module Wasp.Cli.Command.Db.Migrate
  ( migrateDev,
    copyDbMigrationsDir,
    MigrationDirCopyDirection (..),
  )
where

import Control.Monad.Catch (catch)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
-- Wasp generator interface.

import qualified Path as P
import qualified Path.IO as PathIO
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath.Path as SP.Path
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Common (WaspProjectDir, dbMigrationsDirInWaspProjectDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator (dbMigrationsDirInDbRootDir, dbRootDirInProjectRootDir)
import qualified Wasp.Generator.DbGenerator.Operations as DbOps

migrateDev :: Command ()
migrateDev = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let genProjectRootDir =
        waspProjectDir
          </> Cli.Common.dotWaspDirInWaspProjectDir
          </> Cli.Common.generatedCodeDirInDotWaspDir

  -- TODO(matija): It might make sense that this (copying migrations folder from source to
  -- the generated proejct) is responsibility of the generator. Since migrations can also be
  -- considered part of a "source" code, then generator could take care of it and this command
  -- wouldn't have to deal with it. We opened an issue on Github about this.
  --
  -- NOTE(matija): we need to copy migrations down before running "migrate dev" to make sure
  -- all the latest migrations are in the generated project (e.g. Wasp dev checked out something
  -- new) - otherwise "dev" would create a new migration for that and we would end up with two
  -- migrations doing the same thing (which might result in conflict, e.g. during db creation).
  waspSaysC "Copying migrations folder from Wasp to Prisma project..."
  copyDbMigrationDir waspProjectDir genProjectRootDir CopyMigDirDown

  waspSaysC "Performing migration..."
  migrateResult <- liftIO $ DbOps.migrateDev genProjectRootDir
  case migrateResult of
    Left migrateError ->
      throwError $ CommandError $ "Migrate dev failed: " <> migrateError
    Right () -> waspSaysC "Migration done."

  waspSaysC "Copying migrations folder from Prisma to Wasp project..."
  copyDbMigrationDir waspProjectDir genProjectRootDir CopyMigDirUp

  waspSaysC "All done!"
  where
    copyDbMigrationDir waspProjectDir genProjectRootDir copyDirection = do
      copyDbMigDirResult <-
        liftIO $ copyDbMigrationsDir copyDirection waspProjectDir genProjectRootDir
      case copyDbMigDirResult of
        Nothing -> waspSaysC "Done copying migrations folder."
        Just err -> throwError $ CommandError $ "Copying migration folder failed: " ++ err

data MigrationDirCopyDirection = CopyMigDirUp | CopyMigDirDown deriving (Eq)

-- | Copy migrations directory between Wasp source and the generated project.
copyDbMigrationsDir ::
  -- | Copy direction (source -> gen or gen-> source)
  MigrationDirCopyDirection ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  IO (Maybe String)
copyDbMigrationsDir copyDirection waspProjectDir genProjectRootDir = do
  -- Migration folder in Wasp source (seen by Wasp dev and versioned).
  let dbMigrationsDirInWaspProjectDirAbsPath = waspProjectDir </> dbMigrationsDirInWaspProjectDir

  -- Migration folder in the generated code.
  let dbMigrationsDirInGenProjectDirAbsPath =
        genProjectRootDir </> dbRootDirInProjectRootDir
          </> dbMigrationsDirInDbRootDir

  let srcPathAbsDir =
        if copyDirection == CopyMigDirUp
          then SP.Path.toPathAbsDir dbMigrationsDirInGenProjectDirAbsPath
          else SP.Path.toPathAbsDir dbMigrationsDirInWaspProjectDirAbsPath

  let targetPathAbsDir =
        if copyDirection == CopyMigDirUp
          then SP.Path.toPathAbsDir dbMigrationsDirInWaspProjectDirAbsPath
          else SP.Path.toPathAbsDir dbMigrationsDirInGenProjectDirAbsPath

  doesSrcDirExist <- PathIO.doesDirExist srcPathAbsDir
  if doesSrcDirExist
    then
      PathIO.copyDirRecur srcPathAbsDir targetPathAbsDir >> return Nothing
        `catch` (\e -> return $ Just $ show (e :: P.PathException))
        `catch` (\e -> return $ Just $ show (e :: IOError))
    else return Nothing
