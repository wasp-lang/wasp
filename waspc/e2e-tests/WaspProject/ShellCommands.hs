{-# LANGUAGE TypeSynonymInstances #-}

module WaspProject.ShellCommands
  ( WaspProjectContext (..),
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliDbStart,
    waspCliDbReset,
    waspCliDbSeed,
    waspCliCompile,
    waspCliDbMigrateDevDev,
    waspCliBuild,
    waspCliBuildStart,
    waspCliStart,
    waspCliClean,
    waspCliStudio,
    waspCliDbStudio,
    waspCliInfo,
    createSeedFile,
    replaceMainWaspFile,
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
    createFile,
    (~&&),
    (~?),
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.FilePath (joinPath)
import Wasp.Generator.DbGenerator.Common
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)
import WaspProject.FileSystem (seedsDirInWaspProjectDir)

-- | Context for commands which are run from inside of a Wasp app project.
data WaspProjectContext = WaspProjectContext
  { _waspProjectDir :: Path' Abs (Dir WaspProjectDir),
    _waspProjectName :: String
  }

waspCliCompile :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliCompile = return "wasp-cli compile"

waspCliStart :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliStart = return "wasp-cli start"

waspCliBuild :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliBuild = return "wasp-cli build"

-- TODO: improve args situation
waspCliBuildStart :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliBuildStart args = return $ "wasp-cli build start " ++ args

waspCliClean :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliClean = return "wasp-cli clean"

waspCliDbStart :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbStart = return "wasp-cli db start"

-- | We make the migration name deterministic by forcing it to be
-- @no-date-<migrationName>@, instead of usual @<date>-<migrationName>@.
-- This is important for snapshot testing as we don't want a different migration name each time.
-- Caveat: this does mean that we can't have two migrations with the same name in a project.
waspCliDbMigrateDevDev :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbMigrateDevDev migrationName = do
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
    -- NOTE: We supress the `mv` error, because if we call `wasp db migrate-dev` 
    -- when there is nothing to migrate, it succeeds but creates no files.
    replaceMigrationDatePrefix :: FilePath -> ShellCommand
    replaceMigrationDatePrefix migrationDirPath =
      "mv " ++ joinPath [migrationDirPath, "*" ++ migrationName] ++ " " ++ joinPath [migrationDirPath, "no-date-" ++ migrationName] ++ " 2>/dev/null || true"

waspCliDbSeed :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbSeed seedName = return $ "wasp-cli db seed " ++ seedName

waspCliDbReset :: Bool -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbReset reset = return $ "expect -c 'spawn wasp-cli db reset; expect \"?\"; send \"" ++ (if reset then "y" else "n") ++ "\r\"; interact'"

waspCliDbStudio :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbStudio = return "wasp-cli db studio"

waspCliInfo :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliInfo = return "wasp-cli info"

waspCliDockerfile :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDockerfile = return "wasp-cli dockerfile"

waspCliStudio :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliStudio = return "wasp-cli studio"

-- NOTE: fragile, assumes line numbers do not change.
setWaspDbToPSQL :: ShellCommandBuilder WaspProjectContext ShellCommand
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

appendToPrismaFile :: FilePath -> ShellCommandBuilder WaspProjectContext ShellCommand
appendToPrismaFile = appendToFile "schema.prisma"

createSeedFile :: String -> String -> ShellCommandBuilder WaspProjectContext ShellCommand
createSeedFile fileName content = do
  waspProjectContext <- ask
  let seedDir = _waspProjectDir waspProjectContext </> seedsDirInWaspProjectDir

  createFile seedDir fileName content

replaceMainWaspFile :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
replaceMainWaspFile content = do
  waspProjectContext <- ask
  let waspProjectDir = _waspProjectDir waspProjectContext

  createFile waspProjectDir "main.wasp" content

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
