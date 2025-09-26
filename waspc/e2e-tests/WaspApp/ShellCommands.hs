{-# LANGUAGE TypeSynonymInstances #-}

module WaspApp.ShellCommands
  ( WaspAppContext (..),
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
    validateWaspAppDockerImageBuilds,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    appendToFile,
    replaceLineInFile,
    ($&&),
    ($?),
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.FilePath (joinPath)
import WaspApp.FileSystem (WaspAppDir, waspAppBuildDirInWaspAppDir, waspAppMigrationsDirInWaspAppDir, waspAppMigrationsDirInWaspAppOutDir, waspAppOutDirInWaspAppDir)

-- | Context for commands which are run from inside of a Wasp app project.
-- For snapshot tests, commands executed with this context are run from the 'SnapshotTestCommon.SnapshotWaspAppDir' directory.
data WaspAppContext = WaspAppContext
  { _waspAppAbsDir :: Path' Abs (Dir WaspAppDir),
    _waspAppName :: String
  }

-- NOTE: fragile, assumes line numbers do not change.
setWaspDbToPSQL :: ShellCommandBuilder WaspAppContext ShellCommand
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

appendToPrismaFile :: FilePath -> ShellCommandBuilder WaspAppContext ShellCommand
appendToPrismaFile = appendToFile "schema.prisma"

waspCliCompile :: ShellCommandBuilder WaspAppContext ShellCommand
waspCliCompile = return "wasp-cli compile"

waspCliBuild :: ShellCommandBuilder WaspAppContext ShellCommand
waspCliBuild = return "wasp-cli build"

-- | Builds and deletes the Docker image for a Wasp app.
-- Can be disabled via the @WASP_E2E_TESTS_SKIP_DOCKER@ environment variable.
validateWaspAppDockerImageBuilds :: ShellCommandBuilder WaspAppContext ShellCommand
validateWaspAppDockerImageBuilds = do
  waspAppContext <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ _waspAppName waspAppContext
      waspAppBuildDirPath = fromAbsDir (_waspAppAbsDir waspAppContext </> waspAppBuildDirInWaspAppDir)
   in return $
        "[ -z \"$WASP_E2E_TESTS_SKIP_DOCKER\" ]"
          $? "pushd " ++ waspAppBuildDirPath
            $&& "docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t " ++ dockerImageTag ++ " ."
            $&& "docker image rm " ++ dockerImageTag
            $&& "popd"

-- | We make the migration name deterministic by forcing it to be
-- @no-date-<migrationName>@, instead of usual @<date>-<migrationName>@.
-- This is important for snapshot testing as we don't want a different migration name each time.
-- Caveat: this does mean that we can't have two migrations with the same name in a project.
waspCliMigrate :: String -> ShellCommandBuilder WaspAppContext ShellCommand
waspCliMigrate migrationName = do
  waspAppContext <- ask
  let waspAppAbsDir = _waspAppAbsDir waspAppContext
      waspMigrationsDir = fromAbsDir (waspAppAbsDir </> waspAppMigrationsDirInWaspAppDir)
      waspOutMigrationsDir = fromAbsDir (waspAppAbsDir </> waspAppOutDirInWaspAppDir </> waspAppMigrationsDirInWaspAppOutDir)
   in return $
        "wasp-cli db migrate-dev --name " ++ migrationName
          $&& replaceMigrationDatePrefix waspMigrationsDir
          $&& replaceMigrationDatePrefix waspOutMigrationsDir
  where
    replaceMigrationDatePrefix migrationDir =
      "mv " ++ joinPath [migrationDir, "*" ++ migrationName] ++ " " ++ joinPath [migrationDir, "no-date-" ++ migrationName]
