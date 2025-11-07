{-# LANGUAGE TypeSynonymInstances #-}

module WaspProject.ShellCommands
  ( WaspProjectContext (..),
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliDbStart,
    waspCliDbReset,
    waspCliDbSeed,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
    waspCliInfo,
    waspCliDockerfile,
    buildAndRemoveWaspProjectDockerImage,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    appendToFile,
    replaceLineInFile,
    (~&&),
    (~?), createFile,
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>), parseRelDir)
import System.FilePath (joinPath)
import Wasp.Generator.DbGenerator.Common
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)
import Data.Maybe (fromJust)

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

-- TODO:
-- add script file
-- add seed to wasp config
{-
1. Solution - simple
Add all seed scripts at once, assume no existing "seeds" field.

Cases:
- no db field -> modify "content" to have db
- else -> just append seeds with field

Bad:
- wont work on projects with existing seeds

2. Solution - more complex
Add all seed scripts one per one

Cases:
- no db field -> add db field
- no seeds -> add seeds field + seed without comma
- existing seeds -> prepend to start of array with comma

Bad:
- complex
-}


createSeedScript :: String -> String -> ShellCommandBuilder WaspProjectContext ShellCommand
createSeedScript seedName seedContent = do
  waspProjectContext <- ask
  let seedDirRelPath = "src/db/seeds"
      seedDir = _waspProjectDir waspProjectContext </> fromJust (parseRelDir seedDirRelPath)
      seedImportStatement = unwords ["import {", seedName, "} from \"@src/", seedDirRelPath, "/", seedName, "\""]

  createFile seedDir seedName seedContent

  -- find "db: {", if not found write db field:
  -- db: {
  -- }

  -- find "seeds: [", if not found seeds field with first seed:
  -- seeds: [
  --   import { someSeed } from "@src/db/seeds/someSeed"
  -- ]
  -- else, prepend seed with comma to start of array
  -- seeds: [
  --   import { newerSeed } from "@src/db/seeds/newerSeed",
  --   import { someSeed } from "@src/db/seeds/someSeed"
  -- ]

waspCliDbStart :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbStart = return "wasp-cli db start"

waspCliDbReset :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbReset = return "wasp-cli db reset"

waspCliDbSeed :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbSeed = return "wasp-cli db seed"

waspCliCompile :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliCompile = return "wasp-cli compile"

waspCliBuild :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliBuild = return "wasp-cli build"

waspCliInfo :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliInfo = return "wasp-cli info"

waspCliDockerfile :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDockerfile = return "wasp-cli dockerfile"

-- | Builds and deletes the Docker image for a Wasp app.
-- Can be disabled via the @WASP_E2E_TESTS_SKIP_DOCKER@ environment variable.
buildAndRemoveWaspProjectDockerImage :: ShellCommandBuilder WaspProjectContext ShellCommand
buildAndRemoveWaspProjectDockerImage = do
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
