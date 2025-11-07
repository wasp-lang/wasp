{-# LANGUAGE TypeSynonymInstances #-}

module WaspProject.TestCommands
  ( WaspProjectContext (..),
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
    validateWaspProjectDockerImageBuilds,
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Data.List (isSuffixOf)
import Data.Maybe (isNothing)
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.Directory
  ( doesDirectoryExist,
    listDirectory,
    renamePath,
    withCurrentDirectory,
  )
import System.Environment (lookupEnv)
import qualified System.FilePath as FP
import qualified System.Process
import TestCommands
  ( TestCommand (..),
    appendToFile,
    replaceLineInFile,
    shellCommand,
  )
import Wasp.Generator.DbGenerator.Common
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- | Context for commands which are run from inside of a Wasp app project.
data WaspProjectContext = WaspProjectContext
  { _waspProjectDir :: Path' Abs (Dir WaspProjectDir),
    _waspProjectName :: String
  }

-- NOTE: fragile, assumes line numbers do not change.
setWaspDbToPSQL :: TestCommand WaspProjectContext ()
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

appendToPrismaFile :: FilePath -> TestCommand WaspProjectContext ()
appendToPrismaFile = appendToFile "schema.prisma"

waspCliCompile :: TestCommand WaspProjectContext ()
waspCliCompile = shellCommand "wasp-cli compile"

waspCliBuild :: TestCommand WaspProjectContext ()
waspCliBuild = shellCommand "wasp-cli build"

-- | Builds and deletes the Docker image for a Wasp app.
-- Can be disabled via the @WASP_E2E_TESTS_SKIP_DOCKER@ environment variable.
validateWaspProjectDockerImageBuilds :: TestCommand WaspProjectContext ()
validateWaspProjectDockerImageBuilds = do
  waspProjectContext <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ _waspProjectName waspProjectContext
  let waspProjectDir = _waspProjectDir waspProjectContext
  let buildDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

  liftIO $ do
    skipDocker <- lookupEnv "WASP_E2E_TESTS_SKIP_DOCKER"
    when (isNothing skipDocker) $
      withCurrentDirectory (fromAbsDir buildDir) $ do
        let buildCmd =
              "docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t "
                ++ dockerImageTag
                ++ " ."
        let rmCmd = "docker image rm " ++ dockerImageTag
        -- Execute docker commands directly with callCommand since we're in IO
        System.Process.callCommand buildCmd
        System.Process.callCommand rmCmd

-- | We make the migration name deterministic by forcing it to be
-- @no-date-<migrationName>@, instead of usual @<date>-<migrationName>@.
-- This is important for snapshot testing as we don't want a different migration name each time.
-- Caveat: this does mean that we can't have two migrations with the same name in a project.
waspCliMigrate :: String -> TestCommand WaspProjectContext ()
waspCliMigrate migrationName = do
  waspProjectContext <- ask
  let waspMigrationsDir = _waspProjectDir waspProjectContext </> dbMigrationsDirInWaspProjectDir
  let waspOutMigrationsDir =
        _waspProjectDir waspProjectContext
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir
          </> dbRootDirInProjectRootDir
          </> dbMigrationsDirInDbRootDir

  -- Run the migration command
  shellCommand $ "wasp-cli db migrate-dev --name " ++ migrationName

  -- Rename migration directories in both locations
  liftIO $ do
    renameMigrationDir (fromAbsDir waspMigrationsDir) migrationName
    renameMigrationDir (fromAbsDir waspOutMigrationsDir) migrationName

-- | Helper function to rename a migration directory from date prefix to "no-date" prefix.
-- Finds a directory ending with the migration name and renames it to "no-date-<migrationName>".
renameMigrationDir :: FilePath -> String -> IO ()
renameMigrationDir migrationDirPath migrationName = do
  dirExists <- doesDirectoryExist migrationDirPath
  when dirExists $ do
    entries <- listDirectory migrationDirPath
    -- Find directory that ends with the migration name (format is usually: <date>-<migrationName>)
    let matchingDirs = filter (isSuffixOf migrationName) entries
    case matchingDirs of
      (matchingDir : _) -> do
        let oldPath = migrationDirPath FP.</> matchingDir
        let newPath = migrationDirPath FP.</> ("no-date-" ++ migrationName)
        renamePath oldPath newPath
      [] -> return () -- No matching directory found, nothing to rename
