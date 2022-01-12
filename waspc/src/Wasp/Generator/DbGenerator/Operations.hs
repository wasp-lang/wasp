module Wasp.Generator.DbGenerator.Operations
  ( migrateDevAndCopyToSource,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Catch (catch)
import qualified Path as P
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import Wasp.Common (DbMigrationsDir)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator (dbMigrationsDirInDbRootDir, dbRootDirInProjectRootDir)
import qualified Wasp.Generator.DbGenerator.Jobs as DbJobs
import Wasp.Generator.FileDraft.WriteableMonad
  ( WriteableMonad (copyDirectoryRecursive),
  )
import Wasp.Generator.Job (JobMessage)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (printJobMessage)

printJobMsgsUntilExitReceived :: Chan JobMessage -> IO ()
printJobMsgsUntilExitReceived chan = do
  jobMsg <- readChan chan
  case J._data jobMsg of
    J.JobOutput {} -> printJobMessage jobMsg >> printJobMsgsUntilExitReceived chan
    J.JobExit {} -> return ()

-- | Migrates in the generated project context and then copies the migrations dir back
-- up to the wasp project dir to ensure they remain in sync.
migrateDevAndCopyToSource :: Path' Abs (Dir DbMigrationsDir) -> Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
migrateDevAndCopyToSource dbMigrationsDirInWaspProjectDirAbs genProjectRootDirAbs = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (printJobMsgsUntilExitReceived chan)
      (DbJobs.migrateDev genProjectRootDirAbs chan)
  case dbExitCode of
    ExitSuccess -> copyMigrationsBackToSource genProjectRootDirAbs dbMigrationsDirInWaspProjectDirAbs
    ExitFailure code -> return $ Left $ "Migrate (dev) failed with exit code: " ++ show code

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
