{-# LANGUAGE TypeSynonymInstances #-}

module WaspProject.ShellCommands
  ( WaspProjectContext (..),
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
    validateWaspProjectDockerImageBuilds,
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
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- | Context for commands which are run from inside of a Wasp app project.
data WaspProjectContext = WaspProjectContext
  { _waspProjectDir :: Path' Abs (Dir WaspProjectDir),
    _waspProjectName :: String
  }

-- NOTE: fragile, assumes line numbers do not change.
setWaspDbToPSQL :: ShellCommandBuilder WaspProjectContext ShellCommand
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

appendToPrismaFile :: FilePath -> ShellCommandBuilder WaspProjectContext ShellCommand
appendToPrismaFile = appendToFile "schema.prisma"

waspCliCompile :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliCompile = return "wasp-cli compile"

waspCliBuild :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliBuild = return "wasp-cli build"

-- | Builds and deletes the Docker image for a Wasp app.
-- Can be disabled via the @WASP_E2E_TESTS_SKIP_DOCKER@ environment variable.
validateWaspProjectDockerImageBuilds :: ShellCommandBuilder WaspProjectContext ShellCommand
validateWaspProjectDockerImageBuilds = do
  waspProjectContext <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ _waspProjectName waspProjectContext
      waspProjectDir = _waspProjectDir waspProjectContext
   in return $
        "[ -z \"$WASP_E2E_TESTS_SKIP_DOCKER\" ]"
          ~? "cd "
          ++ fromAbsDir (waspProjectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir)
            ~&& "docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t "
          ++ dockerImageTag
          ++ " ."
            ~&& "docker image rm "
          ++ dockerImageTag
            ~&& "cd "
          ++ fromAbsDir waspProjectDir

-- | We make the migration name deterministic by forcing it to be
-- @no-date-<migrationName>@, instead of usual @<date>-<migrationName>@.
-- This is important for snapshot testing as we don't want a different migration name each time.
-- Caveat: this does mean that we can't have two migrations with the same name in a project.
waspCliMigrate :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliMigrate migrationName = do
  waspProjectContext <- ask
  let waspMigrationsDir = _waspProjectDir waspProjectContext </> dbMigrationsDirInWaspProjectDir
      waspOutMigrationsDir =
        _waspProjectDir waspProjectContext
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir
          </> dbRootDirInProjectRootDir
          </> dbMigrationsDirInDbRootDir
   in return $
        "wasp-cli db migrate-dev --name "
          ++ migrationName
            ~&& replaceMigrationDatePrefix (fromAbsDir waspMigrationsDir)
            ~&& replaceMigrationDatePrefix (fromAbsDir waspOutMigrationsDir)
  where
    replaceMigrationDatePrefix migrationDirPath =
      "mv " ++ joinPath [migrationDirPath, "*" ++ migrationName] ++ " " ++ joinPath [migrationDirPath, "no-date-" ++ migrationName]
