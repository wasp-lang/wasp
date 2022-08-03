module Wasp.Generator.DbGenerator.Operations
  ( migrateDevAndCopyToSource,
    generatePrismaClient,
    areAllMigrationsApplied,
    doesSchemaMatchDb,
    writeDbSchemaChecksumToFile,
    isDbMissing,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (when)
import Control.Monad.Catch (catch)
import qualified Data.Text as T
import qualified Path as P
import StrongPath (Abs, Dir, File', Path', Rel)
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
    dbSchemaChecksumOnLastGenerateFileProjectRootDir,
    dbSchemaChecksumOnLastMigrateFileProjectRootDir,
    dbSchemaFileInProjectRootDir,
  )
import qualified Wasp.Generator.DbGenerator.Jobs as DbJobs
import Wasp.Generator.FileDraft.WriteableMonad
  ( WriteableMonad (copyDirectoryRecursive),
  )
import Wasp.Generator.Job (JobMessage)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO
  ( printJobMessage,
    readJobMessagesAndPrintThemPrefixed,
    readJobMessagesAndReturnTextOutput,
  )
import qualified Wasp.Generator.WriteFileDrafts as Generator.WriteFileDrafts
import Wasp.Util (checksumFromFilePath, hexToString)

printJobMsgsUntilExitReceived :: Chan JobMessage -> IO ()
printJobMsgsUntilExitReceived chan = do
  jobMsg <- readChan chan
  case J._data jobMsg of
    J.JobOutput {} -> printJobMessage jobMsg >> printJobMsgsUntilExitReceived chan
    J.JobExit {} -> return ()

-- | Migrates in the generated project context and then copies the migrations dir back
-- up to the wasp project dir to ensure they remain in sync.
migrateDevAndCopyToSource :: Path' Abs (Dir DbMigrationsDir) -> Path' Abs (Dir ProjectRootDir) -> Maybe String -> IO (Either String ())
migrateDevAndCopyToSource dbMigrationsDirInWaspProjectDirAbs genProjectRootDirAbs maybeMigrationName = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (printJobMsgsUntilExitReceived chan)
      (DbJobs.migrateDev genProjectRootDirAbs maybeMigrationName chan)
  case dbExitCode of
    ExitSuccess -> finalizeMigration genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs
    ExitFailure code -> return $ Left $ "Migrate (dev) failed with exit code: " ++ show code

finalizeMigration :: Path' Abs (Dir ProjectRootDir) -> Path' Abs (Dir DbMigrationsDir) -> IO (Either String ())
finalizeMigration genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs = do
  -- NOTE: We are updating a managed CopyDirFileDraft outside the normal generation process, so we must invalidate the checksum entry for it.
  Generator.WriteFileDrafts.removeFromChecksumFile genProjectRootDirAbs [Right $ SP.castDir dbMigrationsDirInProjectRootDir]
  res <- copyMigrationsBackToSource genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs
  writeDbSchemaChecksumToFile genProjectRootDirAbs (SP.castFile dbSchemaChecksumOnLastMigrateFileProjectRootDir)
  return res
  where
    dbMigrationsDirInProjectRootDir = dbRootDirInProjectRootDir SP.</> dbMigrationsDirInDbRootDir

-- | Copies the DB migrations from the generated project dir back up to theh wasp project dir
copyMigrationsBackToSource :: Path' Abs (Dir ProjectRootDir) -> Path' Abs (Dir DbMigrationsDir) -> IO (Either String ())
copyMigrationsBackToSource genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs =
  do
    copyDirectoryRecursive genProjectMigrationsDir waspMigrationsDir >> return (Right ())
      `catch` (\e -> return $ Left $ show (e :: P.PathException))
      `catch` (\e -> return $ Left $ show (e :: IOError))
  where
    waspMigrationsDir = SP.castDir dbMigrationsDirInWaspProjectDirAbs
    genProjectMigrationsDir = SP.castDir $ genProjectRootDirAbs SP.</> dbRootDirInProjectRootDir SP.</> dbMigrationsDirInDbRootDir

-- | This function assumes the DB schema has been generated, as it will attempt to read it from the generated code.
writeDbSchemaChecksumToFile :: Path' Abs (Dir ProjectRootDir) -> Path' (Rel ProjectRootDir) File' -> IO ()
writeDbSchemaChecksumToFile genProjectRootDirAbs dbSchemaChecksumInProjectRootDir = do
  dbSchemaExists <- doesFileExist dbSchemaFp
  when dbSchemaExists $ do
    checksum <- hexToString <$> checksumFromFilePath dbSchemaFp
    writeFile dbSchemaChecksumFp checksum
  where
    dbSchemaFp = SP.fromAbsFile $ genProjectRootDirAbs SP.</> dbSchemaFileInProjectRootDir
    dbSchemaChecksumFp = SP.fromAbsFile $ genProjectRootDirAbs SP.</> dbSchemaChecksumInProjectRootDir

generatePrismaClient :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
generatePrismaClient genProjectRootDirAbs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (DbJobs.generatePrismaClient genProjectRootDirAbs chan)
  case dbExitCode of
    ExitSuccess -> do
      writeDbSchemaChecksumToFile genProjectRootDirAbs (SP.castFile dbSchemaChecksumOnLastGenerateFileProjectRootDir)
      return $ Right ()
    ExitFailure code -> return $ Left $ "Prisma client generation failed with exit code: " ++ show code

areAllMigrationsApplied :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe Bool)
areAllMigrationsApplied genProjectRootDirAbs =
  doesMigrateStatusContainText genProjectRootDirAbs "Database schema is up to date!"

isDbMissing :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe Bool)
isDbMissing genProjectRootDirAbs =
  doesMigrateStatusContainText genProjectRootDirAbs "P1003:"

doesMigrateStatusContainText :: Path' Abs (Dir ProjectRootDir) -> T.Text -> IO (Maybe Bool)
doesMigrateStatusContainText genProjectRootDirAbs desiredTextOutput = do
  let connectionErrorText = "Database connection error:"
  chan <- newChan
  (jobOutput, dbExitCode) <-
    concurrently
      (readJobMessagesAndReturnTextOutput chan)
      (DbJobs.migrateStatus genProjectRootDirAbs chan)
  case dbExitCode of
    ExitSuccess -> do
      if desiredTextOutput `isIn` jobOutput
        then return $ Just True
        else
          if connectionErrorText `isIn` jobOutput
            then return Nothing
            else return $ Just False
    ExitFailure _ -> return Nothing
  where
    needleText `isIn` haystackTextList = any (needleText `T.isInfixOf`) haystackTextList

doesSchemaMatchDb :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe Bool)
doesSchemaMatchDb genProjectRootDirAbs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (readJobMessagesAndReturnTextOutput chan)
      (DbJobs.migrateDiff genProjectRootDirAbs chan)
  -- Schema in sync: 0, Error: 1, Schema differs: 2
  case dbExitCode of
    ExitSuccess -> do
      return $ Just True
    ExitFailure 2 -> return $ Just False
    ExitFailure _ -> return Nothing
