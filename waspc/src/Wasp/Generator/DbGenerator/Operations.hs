module Wasp.Generator.DbGenerator.Operations
  ( migrateDevAndCopyToSource,
    generatePrismaClients,
    doesSchemaMatchDb,
    writeDbSchemaChecksumToFile,
    areAllMigrationsAppliedToDb,
  )
where

import Control.Applicative (liftA2)
import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Catch (catch)
import Control.Monad.Extra (whenM)
import qualified Path as P
import StrongPath (Abs, Dir, File, Path', Rel, (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common
  ( MigrateArgs,
    RefreshOnLastDbConcurrenceChecksumFile (..),
    dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
    dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir,
    dbSchemaChecksumOnLastGenerateFileProjectRootDir,
    dbSchemaFileInProjectRootDir,
    getOnLastDbConcurrenceChecksumFileRefreshAction,
    serverPrismaClientOutputDirEnv,
    webAppPrismaClientOutputDirEnv,
  )
import qualified Wasp.Generator.DbGenerator.Jobs as DbJobs
import Wasp.Generator.FileDraft.WriteableMonad
  ( WriteableMonad (copyDirectoryRecursive),
  )
import Wasp.Generator.Job (JobMessage)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (printJobMessage, readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Generator.WriteFileDrafts as Generator.WriteFileDrafts
import Wasp.Util (checksumFromFilePath, hexToString)
import Wasp.Util.IO (doesFileExist, removeFile)
import qualified Wasp.Util.IO as IOUtil

printJobMsgsUntilExitReceived :: Chan JobMessage -> IO ()
printJobMsgsUntilExitReceived chan = do
  jobMsg <- readChan chan
  case J._data jobMsg of
    J.JobOutput {} -> printJobMessage jobMsg >> printJobMsgsUntilExitReceived chan
    J.JobExit {} -> return ()

-- | Migrates in the generated project context and then copies the migrations dir back
-- up to the wasp project dir to ensure they remain in sync.
migrateDevAndCopyToSource :: Path' Abs (Dir DbMigrationsDir) -> Path' Abs (Dir ProjectRootDir) -> MigrateArgs -> IO (Either String ())
migrateDevAndCopyToSource dbMigrationsDirInWaspProjectDirAbs genProjectRootDirAbs migrateArgs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (printJobMsgsUntilExitReceived chan)
      (DbJobs.migrateDev genProjectRootDirAbs migrateArgs chan)
  case dbExitCode of
    ExitSuccess -> finalizeMigration genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs (getOnLastDbConcurrenceChecksumFileRefreshAction migrateArgs)
    ExitFailure code -> return $ Left $ "Migrate (dev) failed with exit code: " ++ show code

finalizeMigration :: Path' Abs (Dir ProjectRootDir) -> Path' Abs (Dir DbMigrationsDir) -> RefreshOnLastDbConcurrenceChecksumFile -> IO (Either String ())
finalizeMigration genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs onLastDbConcurrenceChecksumFileRefreshAction = do
  -- NOTE: We are updating a managed CopyDirFileDraft outside the normal generation process, so we must invalidate the checksum entry for it.
  Generator.WriteFileDrafts.removeFromChecksumFile genProjectRootDirAbs [Right $ SP.castDir dbMigrationsDirInProjectRootDir]
  res <- copyMigrationsBackToSource genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs
  applyOnLastDbConcurrenceChecksumFileRefreshAction
  return res
  where
    dbMigrationsDirInProjectRootDir = dbRootDirInProjectRootDir </> dbMigrationsDirInDbRootDir
    applyOnLastDbConcurrenceChecksumFileRefreshAction =
      case onLastDbConcurrenceChecksumFileRefreshAction of
        WriteOnLastDbConcurrenceChecksumFile ->
          writeDbSchemaChecksumToFile genProjectRootDirAbs dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir
        RemoveOnLastDbConcurrenceChecksumFile ->
          removeDbSchemaChecksumFile genProjectRootDirAbs dbSchemaChecksumOnLastDbConcurrenceFileProjectRootDir
        IgnoreOnLastDbConcurrenceChecksumFile -> return ()

-- | Copies the DB migrations from the generated project dir back up to theh wasp project dir
copyMigrationsBackToSource :: Path' Abs (Dir ProjectRootDir) -> Path' Abs (Dir DbMigrationsDir) -> IO (Either String ())
copyMigrationsBackToSource genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs =
  copyDirectoryRecursive genProjectMigrationsDir waspMigrationsDir >> return (Right ())
    `catch` (\e -> return $ Left $ show (e :: P.PathException))
    `catch` (\e -> return $ Left $ show (e :: IOError))
  where
    waspMigrationsDir = dbMigrationsDirInWaspProjectDirAbs
    genProjectMigrationsDir = genProjectRootDirAbs </> dbRootDirInProjectRootDir </> dbMigrationsDirInDbRootDir

-- | This function assumes the DB schema has been generated, as it will attempt to read it from the generated code.
writeDbSchemaChecksumToFile :: Path' Abs (Dir ProjectRootDir) -> Path' (Rel ProjectRootDir) (File f) -> IO ()
writeDbSchemaChecksumToFile genProjectRootDirAbs dbSchemaChecksumInProjectRootDir = do
  whenM (doesFileExist dbSchemaFile) $ do
    checksum <- hexToString <$> checksumFromFilePath dbSchemaFile
    IOUtil.writeFile dbSchemaChecksumFile checksum
  where
    dbSchemaFile = genProjectRootDirAbs </> dbSchemaFileInProjectRootDir
    dbSchemaChecksumFile = genProjectRootDirAbs </> dbSchemaChecksumInProjectRootDir

removeDbSchemaChecksumFile :: Path' Abs (Dir ProjectRootDir) -> Path' (Rel ProjectRootDir) (File f) -> IO ()
removeDbSchemaChecksumFile genProjectRootDirAbs dbSchemaChecksumInProjectRootDir = removeFile dbSchemaChecksumFp
  where
    dbSchemaChecksumFp = genProjectRootDirAbs </> dbSchemaChecksumInProjectRootDir

generatePrismaClients :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
generatePrismaClients = liftA2 (>>) generatePrismaClientForServer generatePrismaClientForWebApp
  where
    generatePrismaClientForServer = generatePrismaClient serverPrismaClientOutputDirEnv J.Server
    generatePrismaClientForWebApp = generatePrismaClient webAppPrismaClientOutputDirEnv J.WebApp

generatePrismaClient ::
  (String, String) ->
  J.JobType ->
  Path' Abs (Dir ProjectRootDir) ->
  IO (Either String ())
generatePrismaClient moduleRootDir jobType genProjectRootDirAbs = do
  chan <- newChan
  (_, exitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (DbJobs.generatePrismaClient genProjectRootDirAbs moduleRootDir jobType chan)
  case exitCode of
    ExitSuccess -> do
      writeDbSchemaChecksumToFile genProjectRootDirAbs dbSchemaChecksumOnLastGenerateFileProjectRootDir
      return $ Right ()
    ExitFailure code -> return $ Left $ "Prisma client generation failed with exit code: " ++ show code

-- | Checks `prisma migrate diff` exit code to determine if schema.prisma is
-- different than the DB. Returns Nothing on error as we do not know the current state.
-- Returns Just True if schema.prisma is the same as DB, Just False if it is different, and
-- Nothing if the check itself failed (exe: if a connection to the DB could not be established).
-- NOTE: Here we only compare the schema to the DB, and not the migrations dir.
doesSchemaMatchDb :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe Bool)
doesSchemaMatchDb genProjectRootDirAbs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (DbJobs.migrateDiff genProjectRootDirAbs chan)
  -- Schema in sync: 0, Error: 1, Schema differs: 2
  case dbExitCode of
    ExitSuccess -> return $ Just True
    ExitFailure 2 -> return $ Just False
    ExitFailure _ -> return Nothing

-- | Checks `prisma migrate status` exit code to determine if migrations dir
-- matches the DB. Returns Nothing on error as we do not know the current state.
-- Returns Just True if all migrations are applied. Due to the fact the command
-- returns an error on connection or unapplied migrations, Just False is never returned.
-- It is recommended to call this after some check that confirms DB connectivity, like `doesSchemaMatchDb`.
areAllMigrationsAppliedToDb :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe Bool)
areAllMigrationsAppliedToDb genProjectRootDirAbs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (DbJobs.migrateStatus genProjectRootDirAbs chan)
  case dbExitCode of
    ExitSuccess -> return $ Just True
    ExitFailure _ -> return Nothing
