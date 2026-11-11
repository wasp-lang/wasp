module Wasp.Generator.DbGenerator.Operations
  ( migrateDevAndCopyToSource,
    generatePrismaClient,
    formatSchema,
    checkIfSchemaMatchesDb,
    SchemaDbMatch (..),
    writeDbSchemaChecksumToFile,
    areAllMigrationsAppliedToDb,
    dbReset,
    testDbConnection,
    isDbConnectionPossible,
    prismaErrorContainsDbNotCreatedError,
  )
where

import Control.Monad.Catch (catch)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Path as P
import StrongPath (Abs, Dir, File, Path', Rel, (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import qualified Text.Regex.TDFA as TR
import Wasp.Env (EnvVar)
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
import qualified Wasp.Job as Job
import Wasp.Project.Db.Migrations (DbMigrationsDir)
import Wasp.Util (checkExitSuccess, checksumFromFilePath, hexToString)
import Wasp.Util.IO (deleteFileIfExists, doesFileExist)
import qualified Wasp.Util.IO as IOUtil

data DbConnectionTestResult
  = DbConnectionSuccess
  | DbNotCreated
  | DbConnectionFailure
  deriving (Eq)

-- | Migrates in the generated project context and then copies the migrations dir back
-- up to the wasp project dir to ensure they remain in sync.
migrateDevAndCopyToSource :: [EnvVar] -> Path' Abs (Dir DbMigrationsDir) -> Path' Abs (Dir GeneratedAppDir) -> MigrateArgs -> IO (Either String ())
migrateDevAndCopyToSource envVars dbMigrationsDirInWaspProjectDirAbs generatedAppDirAbs migrateArgs = runExceptT $ do
  ExceptT $ checkExitSuccess "Migrate (dev)" <$> Job.run (DbJobs.migrateDev envVars generatedAppDirAbs migrateArgs)
  ExceptT $ finalizeMigration generatedAppDirAbs dbMigrationsDirInWaspProjectDirAbs (getOnLastDbConcurrenceChecksumFileRefreshAction migrateArgs)

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
  [EnvVar] ->
  Path' Abs (Dir GeneratedAppDir) ->
  ResetArgs ->
  IO (Either String ())
dbReset envVars generatedAppDir resetArgs = do
  -- We are doing quite a move here, resetting the whole db, so best to delete the checksum file,
  -- which will force Wasp to do a deep check of migrations next time, just to be sure.
  removeDbSchemaChecksumFile generatedAppDir dbSchemaChecksumOnLastDbConcurrenceFileInGeneratedAppDir
  checkExitSuccess "Resetting the database" <$> Job.run (Job.prefixWith Job.Db $ DbJobs.reset envVars generatedAppDir resetArgs)

testDbConnection ::
  [EnvVar] ->
  Path' Abs (Dir GeneratedAppDir) ->
  IO DbConnectionTestResult
testDbConnection envVars generatedAppDir = do
  (exitCode, output) <- Job.run $ Job.captureOutput $ DbJobs.dbExecuteTest envVars generatedAppDir

  case exitCode of
    ExitSuccess -> return DbConnectionSuccess
    ExitFailure _ -> do
      let databaseNotCreated = prismaErrorContainsDbNotCreatedError output

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
generatePrismaClient generatedAppDir = runExceptT $ do
  ExceptT $ checkExitSuccess "Prisma client generation" <$> Job.run (Job.prefixWith Job.Db $ DbJobs.generatePrismaClient generatedAppDir)
  liftIO updateDbSchemaChecksumOnLastGenerate
  where
    updateDbSchemaChecksumOnLastGenerate =
      writeDbSchemaChecksumToFile generatedAppDir dbSchemaChecksumOnLastGenerateFileInGeneratedAppDir

-- | Formats schema.prisma in place, without printing Prisma's output unless it fails.
formatSchema :: Path' Abs (Dir GeneratedAppDir) -> IO (Either String ())
formatSchema generatedAppDir = do
  (exitCode, output) <- Job.run $ Job.captureOutput $ DbJobs.formatSchema generatedAppDir
  return $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure code -> Left $ "Prisma schema formatting failed with exit code " ++ show code ++ ":\n" ++ T.unpack output

data SchemaDbMatch
  = SchemaMatchesDb
  | SchemaDiffersFromDb
  | DbDoesNotExist
  | SchemaDbMatchCheckFailed
  deriving (Eq, Show)

-- | Runs `prisma migrate diff` to determine if schema.prisma matches the DB.
-- NOTE: Here we only compare the schema to the DB, and not the migrations dir.
checkIfSchemaMatchesDb :: [EnvVar] -> Path' Abs (Dir GeneratedAppDir) -> IO SchemaDbMatch
checkIfSchemaMatchesDb envVars generatedAppDirAbs = Job.run $ Job.prefixWith Job.Db $ do
  (exitCode, output) <- Job.captureOutput $ DbJobs.migrateDiff envVars generatedAppDirAbs
  let schemaDbMatch = case exitCode of
        ExitSuccess -> SchemaMatchesDb
        ExitFailure 2 -> SchemaDiffersFromDb
        ExitFailure _
          | prismaErrorContainsDbNotCreatedError output -> DbDoesNotExist
          | otherwise -> SchemaDbMatchCheckFailed
  -- We print output when check failed to help with figuring out why,
  -- while others are known and therefore handled upstream.
  case schemaDbMatch of
    SchemaMatchesDb -> return ()
    SchemaDiffersFromDb -> return ()
    DbDoesNotExist -> return ()
    SchemaDbMatchCheckFailed -> Job.emitOutput Job.Stderr output
  return schemaDbMatch

-- | Checks `prisma migrate status` exit code to determine if migrations dir
-- matches the DB. Returns Nothing on error as we do not know the current state.
-- Returns Just True if all migrations are applied. Due to the fact the command
-- returns an error on connection or unapplied migrations, Just False is never returned.
-- It is recommended to call this after some check that confirms DB connectivity, like `checkIfSchemaMatchesDb`.
areAllMigrationsAppliedToDb :: [EnvVar] -> Path' Abs (Dir GeneratedAppDir) -> IO (Maybe Bool)
areAllMigrationsAppliedToDb envVars generatedAppDirAbs = do
  dbExitCode <- Job.run $ Job.prefixWith Job.Db $ DbJobs.migrateStatus envVars generatedAppDirAbs
  case dbExitCode of
    ExitSuccess -> return $ Just True
    ExitFailure _ -> return Nothing
