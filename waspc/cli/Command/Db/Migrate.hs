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

    waspSaysC "Checking for changes in schema to save..."
    migrateSaveResult <- liftIO $ DbOps.migrateSave genProjectRootDir migrationName
    case migrateSaveResult of
        Left migrateSaveError -> throwError $ CommandError $ "Migrate save failed: "
                                 ++ migrateSaveError
        Right () -> waspSaysC "Done."

    waspSaysC "Copying migrations folder from Prisma to Wasp project..."
    copyDbMigDirResult <- liftIO $ copyDbMigrationsDir waspProjectDir genProjectRootDir
    case copyDbMigDirResult of
        Nothing -> waspSaysC "Done."
        Just err -> throwError $ CommandError $ "Copying migration folder failed: " ++ err

    applyAvailableMigrationsAndGenerateClient genProjectRootDir

    waspSaysC "All done!"

    where
        copyDbMigrationsDir
            :: Path Abs (Dir WaspProjectDir)
            -> Path Abs (Dir ProjectRootDir)
            -> IO (Maybe String) -- ^ Possibly contains error message.
        copyDbMigrationsDir waspProjectDir genProjectRootDir= do
            let dbMigrationsDirInDbRootDir = SP.fromPathRelDir [P.reldir|migrations|]
            let dbMigrationsDirSrc = genProjectRootDir </> dbRootDirInProjectRootDir
                                     </> dbMigrationsDirInDbRootDir
            let dbMigrationsDirTarget = waspProjectDir </> dbMigrationsDirInDbRootDir
            
            ((PathIO.copyDirRecur (SP.toPathAbsDir dbMigrationsDirSrc)
                                  (SP.toPathAbsDir dbMigrationsDirTarget)) >> return Nothing)
            `catch` (\e -> return $ Just $ show (e :: P.PathException))
            `catch` (\e -> return $ Just $ show (e :: IOError))

migrateUp :: Command ()
migrateUp = do
    waspProjectDir <- findWaspProjectRootDirFromCwd
    let genProjectRootDir = waspProjectDir </> Common.dotWaspDirInWaspProjectDir
                            </> Common.generatedCodeDirInDotWaspDir

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

