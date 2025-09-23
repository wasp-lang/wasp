{-# LANGUAGE TypeSynonymInstances #-}

module WaspApp.ShellCommands
  ( WaspAppContext (..),
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
    buildWaspDockerImage,
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
import System.FilePath (joinPath, (</>))

-- | Context for commands which are run from inside of a Wasp app project.
-- For snapshot tests, commands executed with this context are run from the 'SnapshotTestCommon.SnapshotWaspAppDir' directory.
data WaspAppContext = WaspAppContext
  {_waspAppName :: String}

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
buildWaspDockerImage :: ShellCommandBuilder WaspAppContext ShellCommand
buildWaspDockerImage = do
  waspAppContext <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ _waspAppName waspAppContext
   in return $
        "[ -z \"$WASP_E2E_TESTS_SKIP_DOCKER\" ]"
          $? "cd .wasp/build"
            $&& "docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t " ++ dockerImageTag ++ " ."
            $&& "docker image rm " ++ dockerImageTag
            $&& "cd ../.."

-- | We normalize the migration names to @no-date-<migrationName>@ for reproducibility.
-- This means that we can't generate two migrations with the same name in a project.
waspCliMigrate :: String -> ShellCommandBuilder WaspAppContext ShellCommand
waspCliMigrate migrationName =
  return $
    "wasp-cli db migrate-dev --name " ++ migrationName
      $&& replaceMigrationDatePrefix waspMigrationsDir
      $&& replaceMigrationDatePrefix dotWaspMigrationsDir
  where
    waspMigrationsDir = "migrations"
    dotWaspMigrationsDir = joinPath [".wasp", "out", "db", "migrations"]

    replaceMigrationDatePrefix :: String -> ShellCommand
    replaceMigrationDatePrefix migrationDir =
      "mv " ++ (migrationDir </> ("*" ++ migrationName)) ++ " " ++ (migrationDir </> ("no-date-" ++ migrationName))
