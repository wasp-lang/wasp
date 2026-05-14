module Wasp.Generator.DbGenerator.Operations
  ( migrateDevAndCopyToSource,
    generatePrismaClient,
    doesSchemaMatchDb,
    writeDbSchemaChecksumToFile,
    areAllMigrationsAppliedToDb,
    dbReset,
    dbSeed,
    testDbConnection,
    isDbConnectionPossible,
    prismaErrorContainsDbNotCreatedError,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Catch (catch)
import Control.Monad.Extra (whenM)
import qualified Data.Text as T
import qualified Path as P
import StrongPath (Abs, Dir, File, Path', Rel, (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import qualified Text.Regex.TDFA as TR
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DbGenerator.Common
  ( DbSchemaChecksumFile,
    MigrateArgs,
    RefreshOnLastDbConcurrenceChecksumFile (..),
    ResetArgs,
    dbMigrationsDirInDbRootDir,
    dbRootDirInGeneratedAppDir,
    dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir,
    dbSchemaChecksumOnLastGenerateFileInGeneratedAppDir,
    dbSchemaFileInGeneratedAppDir,
    getOnLastDbConcurrenceChecksumFileRefreshAction,
  )
import qualified Wasp.Generator.DbGenerator.Jobs as DbJobs
import Wasp.Generator.FileDraft.WriteableMonad (WriteableMonad (copyDirectoryRecursive, doesDirectoryExist))
import qualified Wasp.Generator.WriteFileDrafts as Generator.WriteFileDrafts
import Wasp.Job.IO
  ( collectJobTextOutputUntilExitReceived,
    printJobMsgsUntilExitReceived,
    readJobMessagesAndPrintThemPrefixed,
  )
import Wasp.Project.Db.Migrations (DbMigrationsDir)
import Wasp.Util (checksumFromFilePath, hexToString)
import Wasp.Util.IO (deleteFileIfExists, doesFileExist)
import qualified Wasp.Util.IO as IOUtil

data DbConnectionTestResult
  = DbConnectionSuccess
  | DbNotCreated
  | DbConnectionFailure
  deriving (Eq)

-- | Migrates in the generated project context and then copies the migrations dir back
-- up to the wasp project dir to ensure they remain in sync.
migrateDevAndCopyToSource :: Path' Abs (Dir DbMigrationsDir) -> Path' Abs (Dir GeneratedAppDir) -> MigrateArgs -> IO (Either String ())
migrateDevAndCopyToSource dbMigrationsDirInWaspProjectDirAbs generatedAppDirAbs migrateArgs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (printJobMsgsUntilExitReceived chan)
      (DbJobs.migrateDev generatedAppDirAbs migrateArgs chan)
  case dbExitCode of
    ExitSuccess -> finalizeMigration generatedAppDirAbs dbMigrationsDirInWaspProjectDirAbs (getOnLastDbConcurrenceChecksumFileRefreshAction migrateArgs)
    ExitFailure code -> return $ Left $ "Migrate (dev) failed with exit code: " ++ show code

finalizeMigration :: Path' Abs (Dir GeneratedAppDir) -> Path' Abs (Dir DbMigrationsDir) -> RefreshOnLastDbConcurrenceChecksumFile -> IO (Either String ())
finalizeMigration generatedAppDirAbs dbMigrationsDirInWaspProjectDirAbs onLastDbConcurrenceChecksumFileRefreshAction = do
  -- NOTE: We are updating a managed CopyDirFileDraft outside the normal generation process, so we must invalidate the checksum entry for it.
  Generator.WriteFileDrafts.removeFromChecksumFile generatedAppDirAbs [Right $ SP.castDir dbMigrationsDirInGeneratedAppDir]
  res <- copyMigrationsBackToSourceIfTheyExist generatedAppDirAbs dbMigrationsDirInWaspProjectDirAbs
  applyOnLastDbConcurrenceChecksumFileRefreshAction
  return res
  where
    dbMigrationsDirInGeneratedAppDir = dbRootDirInGeneratedAppDir </> dbMigrationsDirInDbRootDir
    applyOnLastDbConcurrenceChecksumFileRefreshAction =
      case onLastDbConcurrenceChecksumFileRefreshAction of
        WriteOnLastDbConcurrenceChecksumFile ->
          writeDbSchemaChecksumToFile generatedAppDirAbs dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir
        RemoveOnLastDbConcurrenceChecksumFile ->
          removeDbSchemaChecksumFile generatedAppDirAbs dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir
        IgnoreOnLastDbConcurrenceChecksumFile -> return ()

-- | Copies the DB migrations from the generated project dir back up to theh wasp project dir
copyMigrationsBackToSourceIfTheyExist ::
  Path' Abs (Dir GeneratedAppDir) ->
  Path' Abs (Dir DbMigrationsDir) ->
  IO (Either String ())
copyMigrationsBackToSourceIfTheyExist generatedAppDirAbs dbMigrationsDirInWaspProjectDirAbs = do
  doesDirectoryExist (SP.fromAbsDir genProjectMigrationsDir) >>= \case
    False -> return $ Right ()
    True -> copyMigrationsDir
  where
    copyMigrationsDir =
      copyDirectoryRecursive genProjectMigrationsDir waspMigrationsDir
        >> return (Right ())
          `catch` (\e -> return $ Left $ show (e :: P.PathException))
          `catch` (\e -> return $ Left $ show (e :: IOError))

    waspMigrationsDir = dbMigrationsDirInWaspProjectDirAbs
    genProjectMigrationsDir = generatedAppDirAbs </> dbRootDirInGeneratedAppDir </> dbMigrationsDirInDbRootDir

-- | This function assumes the DB schema has been generated, as it will attempt to read it from the generated code.
writeDbSchemaChecksumToFile ::
  (DbSchemaChecksumFile f) =>
  Path' Abs (Dir GeneratedAppDir) ->
  Path' (Rel GeneratedAppDir) (File f) ->
  IO ()
writeDbSchemaChecksumToFile generatedAppDirAbs dbSchemaChecksumInGeneratedAppDir = do
  whenM (doesFileExist dbSchemaFile) $ do
    checksum <- hexToString <$> checksumFromFilePath dbSchemaFile
    IOUtil.writeFile dbSchemaChecksumFile checksum
  where
    dbSchemaFile = generatedAppDirAbs </> dbSchemaFileInGeneratedAppDir
    dbSchemaChecksumFile = generatedAppDirAbs </> dbSchemaChecksumInGeneratedAppDir

removeDbSchemaChecksumFile ::
  (DbSchemaChecksumFile f) =>
  Path' Abs (Dir GeneratedAppDir) ->
  Path' (Rel GeneratedAppDir) (File f) ->
  IO ()
removeDbSchemaChecksumFile generatedAppDirAbs dbSchemaChecksumInGeneratedAppDir = deleteFileIfExists dbSchemaChecksumFp
  where
    dbSchemaChecksumFp = generatedAppDirAbs </> dbSchemaChecksumInGeneratedAppDir

-- Resets the database: drops all data and applies all migrations from scratch.
dbReset ::
  Path' Abs (Dir GeneratedAppDir) ->
  ResetArgs ->
  IO (Either String ())
dbReset generatedAppDir resetArgs = do
  -- We are doing quite a move here, resetting the whole db, so best to delete the checksum file,
  -- which will force Wasp to do a deep check of migrations next time, just to be sure.
  removeDbSchemaChecksumFile generatedAppDir dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir
  chan <- newChan
  ((), exitCode) <-
    readJobMessagesAndPrintThemPrefixed chan `concurrently` DbJobs.reset generatedAppDir resetArgs chan
  return $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure c -> Left $ "Failed with exit code " <> show c

dbSeed ::
  Path' Abs (Dir GeneratedAppDir) ->
  String ->
  IO (Either String ())
dbSeed generatedAppDir seedName = do
  chan <- newChan
  ((), exitCode) <-
    readJobMessagesAndPrintThemPrefixed chan `concurrently` DbJobs.seed generatedAppDir seedName chan
  return $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure c -> Left $ "Failed with exit code " <> show c

testDbConnection ::
  Path' Abs (Dir GeneratedAppDir) ->
  IO DbConnectionTestResult
testDbConnection generatedAppDir = do
  chan <- newChan
  exitCode <- DbJobs.dbExecuteTest generatedAppDir chan

  case exitCode of
    ExitSuccess -> return DbConnectionSuccess
    ExitFailure _ -> do
      outputLines <- collectJobTextOutputUntilExitReceived chan
      let databaseNotCreated = any prismaErrorContainsDbNotCreatedError outputLines

      return $
        if databaseNotCreated
          then DbNotCreated
          else DbConnectionFailure

-- Prisma error code for "Database not created" is P1003.
prismaErrorContainsDbNotCreatedError :: T.Text -> Bool
prismaErrorContainsDbNotCreatedError text = text TR.=~ ("\\bP1003\\b" :: String)

isDbConnectionPossible :: DbConnectionTestResult -> Bool
isDbConnectionPossible DbConnectionSuccess = True
isDbConnectionPossible DbNotCreated = True
isDbConnectionPossible _ = False

generatePrismaClient :: Path' Abs (Dir GeneratedAppDir) -> IO (Either String ())
generatePrismaClient generatedAppDir = do
  chan <- newChan
  (_, exitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (DbJobs.generatePrismaClient generatedAppDir chan)
  case exitCode of
    ExitFailure code -> return $ Left $ "Prisma client generation failed with exit code: " ++ show code
    ExitSuccess -> do
      updateDbSchemaChecksumOnLastGenerate
      return $ Right ()
  where
    updateDbSchemaChecksumOnLastGenerate =
      writeDbSchemaChecksumToFile generatedAppDir dbSchemaChecksumOnLastGenerateFileInGeneratedAppDir

-- | Checks `prisma migrate diff` exit code to determine if schema.prisma is
-- different than the DB. Returns Nothing on error as we do not know the current state.
-- Returns Just True if schema.prisma is the same as DB, Just False if it is different, and
-- Nothing if the check itself failed (exe: if a connection to the DB could not be established).
-- NOTE: Here we only compare the schema to the DB, and not the migrations dir.
doesSchemaMatchDb :: Path' Abs (Dir GeneratedAppDir) -> IO (Maybe Bool)
doesSchemaMatchDb generatedAppDirAbs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (DbJobs.migrateDiff generatedAppDirAbs chan)
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
areAllMigrationsAppliedToDb :: Path' Abs (Dir GeneratedAppDir) -> IO (Maybe Bool)
areAllMigrationsAppliedToDb generatedAppDirAbs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (DbJobs.migrateStatus generatedAppDirAbs chan)
  case dbExitCode of
    ExitSuccess -> return $ Just True
    ExitFailure _ -> return Nothing
