module Command.Db.Migrate
    ( migrateSave
    , migrateUp
    ) where

import Control.Monad.Catch (catch)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Path as P
import qualified Path.IO as PathIO

import StrongPath ((</>), Abs, Dir, Path)
import qualified StrongPath as SP
import Command (Command, CommandError(..))
import Command.Common (findWaspProjectRootDirFromCwd, waspSaysC)
import qualified Common
import Common (WaspProjectDir)

-- Wasp generator interface.
import qualified Generator.DbGenerator.Operations as DbOps
import Generator.DbGenerator (dbRootDirInProjectRootDir)
import Generator.Common (ProjectRootDir)


migrateSave :: String -> Command ()
migrateSave migrationName = do
    waspProjectDir <- findWaspProjectRootDirFromCwd
    let genProjectRootDir = waspProjectDir </> Common.dotWaspDirInWaspProjectDir
                            </> Common.generatedCodeDirInDotWaspDir

    -- TODO(matija): It might make sense that this (copying migrations folder from source to
    -- the generated proejct) is responsibility of the generator. Since migrations can also be
    -- considered part of a "source" code, then generator could take care of it and this command
    -- wouldn't have to deal with it. We opened an issue on Github about this.
    --
    -- NOTE(matija): we need to copy migrations down before running "migrate save" to make sure
    -- all the latest migrations are in the generated project (e.g. Wasp dev checked out something
    -- new) - otherwise "save" would create a new migration for that and we would end up with two
    -- migrations doing the same thing (which might result in conflict, e.g. during db creation).
    waspSaysC "Copying migrations folder from Wasp to Prisma project..."
    copyDbMigDirDownResult <- liftIO $ copyDbMigrationsDir CopyMigDirDown waspProjectDir
                                                           genProjectRootDir
    case copyDbMigDirDownResult of
        Nothing -> waspSaysC "Done."
        Just err -> throwError $ CommandError $ "Copying migration folder failed: " ++ err

    waspSaysC "Checking for changes in schema to save..."
    migrateSaveResult <- liftIO $ DbOps.migrateSave genProjectRootDir migrationName
    case migrateSaveResult of
        Left migrateSaveError -> throwError $ CommandError $ "Migrate save failed: "
                                 ++ migrateSaveError
        Right () -> waspSaysC "Done."

    waspSaysC "Copying migrations folder from Prisma to Wasp project..."
    copyDbMigDirUpResult <- liftIO $ copyDbMigrationsDir CopyMigDirUp waspProjectDir
                                                         genProjectRootDir
    case copyDbMigDirUpResult of
        Nothing -> waspSaysC "Done."
        Just err -> throwError $ CommandError $ "Copying migration folder failed: " ++ err

    applyAvailableMigrationsAndGenerateClient genProjectRootDir

    waspSaysC "All done!"

migrateUp :: Command ()
migrateUp = do
    waspProjectDir <- findWaspProjectRootDirFromCwd
    let genProjectRootDir = waspProjectDir </> Common.dotWaspDirInWaspProjectDir
                            </> Common.generatedCodeDirInDotWaspDir

    waspSaysC "Copying migrations folder from Wasp to Prisma project..."
    copyDbMigDirResult <- liftIO $ copyDbMigrationsDir CopyMigDirDown waspProjectDir
                                                       genProjectRootDir
    case copyDbMigDirResult of
        Nothing -> waspSaysC "Done."
        Just err -> throwError $ CommandError $ "Copying migration folder failed: " ++ err

    applyAvailableMigrationsAndGenerateClient genProjectRootDir

    waspSaysC "All done!"

applyAvailableMigrationsAndGenerateClient :: Path Abs (Dir ProjectRootDir) -> Command ()
applyAvailableMigrationsAndGenerateClient genProjectRootDir = do
    waspSaysC "Checking for migrations to apply..."
    migrateUpResult <- liftIO $ DbOps.migrateUp genProjectRootDir
    case migrateUpResult of
        Left migrateUpError -> throwError $ CommandError $ "Migrate up failed: " ++ migrateUpError
        Right () -> waspSaysC "Done."

    waspSaysC "Generating Prisma client..."
    genClientResult <- liftIO $ DbOps.generateClient genProjectRootDir
    case genClientResult of
        Left genClientError -> throwError $ CommandError $ "Generating client failed: " ++
                               genClientError
        Right () -> waspSaysC "Done."


data MigrationDirCopyDirection = CopyMigDirUp | CopyMigDirDown deriving (Eq)

-- | Copy migrations directory between Wasp source and the generated project.
copyDbMigrationsDir
    :: MigrationDirCopyDirection -- ^ Copy direction (source -> gen or gen-> source)
    -> Path Abs (Dir WaspProjectDir)
    -> Path Abs (Dir ProjectRootDir)
    -> IO (Maybe String)
copyDbMigrationsDir copyDirection waspProjectDir genProjectRootDir = do
    let dbMigrationsDirInDbRootDir = SP.fromPathRelDir [P.reldir|migrations|]

    -- Migration folder in Wasp source (seen by Wasp dev and versioned).
    let dbMigrationsDirInWaspProjectDirAbs = waspProjectDir </> dbMigrationsDirInDbRootDir

    -- Migration folder in the generated code.
    let dbMigrationsDirInGenProjectDirAbs = genProjectRootDir </> dbRootDirInProjectRootDir
                                             </> dbMigrationsDirInDbRootDir

    let src = if copyDirection == CopyMigDirUp
              then dbMigrationsDirInGenProjectDirAbs
              else dbMigrationsDirInWaspProjectDirAbs

    let target = if copyDirection == CopyMigDirUp
                 then dbMigrationsDirInWaspProjectDirAbs
                 else dbMigrationsDirInGenProjectDirAbs

    doesSrcDirExist <- PathIO.doesDirExist (SP.toPathAbsDir src)
    if doesSrcDirExist == True then
        ((PathIO.copyDirRecur (SP.toPathAbsDir src)
                              (SP.toPathAbsDir target)) >> return Nothing)
        `catch` (\e -> return $ Just $ show (e :: P.PathException))
        `catch` (\e -> return $ Just $ show (e :: IOError))
        
        else return Nothing

