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
    (~&&),
    (~?),
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.FilePath (joinPath)
import Wasp.Generator.DbGenerator.Common
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- | Context for commands which are run from inside of a Wasp app project.
data WaspAppContext = WaspAppContext
  { _waspAppDir :: Path' Abs (Dir WaspProjectDir),
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
      waspAppDir = _waspAppDir waspAppContext
   in return $
        "[ -z \"$WASP_E2E_TESTS_SKIP_DOCKER\" ]"
          ~? "cd " ++ fromAbsDir (waspAppDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir)
            ~&& "docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t " ++ dockerImageTag ++ " ."
            ~&& "docker image rm " ++ dockerImageTag
            ~&& "cd " ++ fromAbsDir waspAppDir

-- | We make the migration name deterministic by forcing it to be
-- @no-date-<migrationName>@, instead of usual @<date>-<migrationName>@.
-- This is important for snapshot testing as we don't want a different migration name each time.
-- Caveat: this does mean that we can't have two migrations with the same name in a project.
waspCliMigrate :: String -> ShellCommandBuilder WaspAppContext ShellCommand
waspCliMigrate migrationName = do
  waspAppContext <- ask
  let waspAppDir = _waspAppDir waspAppContext
      waspMigrationsDir = waspAppDir </> dbMigrationsDirInWaspProjectDir
      waspBuildMigrationsDir = waspAppDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir </> dbRootDirInProjectRootDir </> dbMigrationsDirInDbRootDir
   in return $
        "wasp-cli db migrate-dev --name " ++ migrationName
          ~&& replaceMigrationDatePrefix (fromAbsDir waspMigrationsDir)
          ~&& replaceMigrationDatePrefix (fromAbsDir waspBuildMigrationsDir)
  where
    replaceMigrationDatePrefix migrationDir =
      "mv " ++ joinPath [migrationDir, "*" ++ migrationName] ++ " " ++ joinPath [migrationDir, "no-date-" ++ migrationName]
