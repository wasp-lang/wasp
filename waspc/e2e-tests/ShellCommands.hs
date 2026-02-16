{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ShellCommands
  ( ShellCommand,
    ShellCommandBuilder (..),
    WaspNewTemplate (..),
    buildShellCommand,
    (~|),
    (~&&),
    (~?),
    (~||),
    createFile,
    appendToFile,
    replaceLineInFile,
    waspCliNewInteractive,
    waspCliNew,
    waspCliCompletion,
    WaspProjectContext (..),
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliDbStart,
    waspCliDbReset,
    waspCliDbSeed,
    waspCliCompile,
    waspCliDbMigrateDev,
    waspCliBuild,
    waspCliBuildStart,
    waspCliStart,
    waspCliClean,
    waspCliStudio,
    waspCliDbStudio,
    waspCliInfo,
    waspCliDeps,
    createSeedFile,
    replaceMainWaspFile,
    waspCliDockerfile,
    buildAndRemoveWaspProjectDockerImage,
    TestContext (..),
    inTestWaspProjectDir,
    createTestWaspProject,
    SnapshotTestContext (..),
    createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
  )
where

import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import FileSystem (GitRootDir, SnapshotDir, TestCaseDir, gitRootFromSnapshotDir, mainWaspFileInWaspProjectDir, seedsDirInWaspProjectDir, seedsFileInSeedsDir)
import StrongPath (Abs, Dir, File', Path', Rel, fromAbsDir, fromAbsFile, fromRelDir, parent, (</>))
import System.FilePath (joinPath)
import Wasp.Generator.DbGenerator.Common (dbMigrationsDirInDbRootDir, dbRootDirInProjectRootDir)
import Wasp.Project (WaspProjectDir)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- NOTE: Using `wasp-cli` herein so we can assume using latest `cabal install` in CI and locally.
-- TODO: In future, find a good way to test `wasp-cli start`.

type ShellCommand = String

-- | Builds shell command with access and assumptions to some context.
-- e.g. 'WaspProjectContext' assumes commands are run from inside a Wasp project.
-- It also provides access to context details like the command execution directory.
newtype ShellCommandBuilder context a = ShellCommandBuilder (Reader context a)
  deriving (Functor, Applicative, Monad, MonadReader context)

buildShellCommand :: context -> ShellCommandBuilder context a -> a
buildShellCommand context (ShellCommandBuilder reader) = runReader reader context

-- Command utilities

-- | Pipe the output of one command into another.
(~|) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 ~| cmd2 = cmd1 ++ " | " ++ cmd2

infixl 7 ~|

-- | Execute the second command only if the first command succeeds.
(~&&) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 ~&& cmd2 = cmd1 ++ " && " ++ cmd2

infixl 6 ~&&

-- | Execute the second command only if the first command fails.
(~||) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 ~|| cmd2 = cmd1 ++ " || " ++ cmd2

infixl 6 ~||

-- | Execute the second command only if the first command succeeds.
-- The command chain will continue regardless of whether the second command runs.
(~?) :: ShellCommand -> ShellCommand -> ShellCommand
(~?) condition command =
  "if " ++ condition ++ "; then " ++ command ++ " ;fi"

infixl 4 ~?

-- General commands

createFile :: Path' Abs File' -> T.Text -> ShellCommandBuilder context ShellCommand
createFile file fileContent = return $ createParentDir ~&& writeContentsToFile
  where
    createParentDir :: ShellCommand
    createParentDir = "mkdir -p " ++ fromAbsDir (parent file)

    writeContentsToFile :: ShellCommand
    writeContentsToFile = "printf %s " ++ base64FileContent ++ " | base64 -d > " ++ fromAbsFile file

    -- Using base64 encoding for file content helps us escape dealing with special characters.
    base64FileContent = C8.unpack . B64.encode . C8.pack . T.unpack $ fileContent

appendToFile :: FilePath -> T.Text -> ShellCommandBuilder context ShellCommand
appendToFile fileName content =
  -- NOTE: Using `show` to preserve newlines in string.
  return $ "printf " ++ show (T.unpack content ++ "\n") ++ " >> " ++ fileName

replaceLineInFile :: FilePath -> Int -> String -> ShellCommandBuilder context ShellCommand
replaceLineInFile fileName lineNumber line =
  return $
    unwords ["awk", "'", ifLineNumberMatches, replaceWholeLine, printCurrentLine, "'", fileName, ">", tempFileName]
      ~&& unwords ["mv", tempFileName, fileName]
  where
    ifLineNumberMatches = "NR==" ++ show lineNumber
    replaceWholeLine = "{$0=" ++ show line ++ "}"
    printCurrentLine = "1"

    tempFileName = fileName ++ ".tmp"

data WaspNewTemplate = Minimal | Basic | SaaS

waspCliNewInteractive :: String -> WaspNewTemplate -> ShellCommandBuilder context ShellCommand
waspCliNewInteractive appName template =
  return $
    unwords ["printf", "\"" ++ appName ++ "\n" ++ templateNumber ++ "\n\""] ~| "wasp-cli new"
  where
    templateNumber = case template of
      Basic -> "1"
      Minimal -> "2"
      SaaS -> "3"

waspCliNew :: String -> WaspNewTemplate -> ShellCommandBuilder context ShellCommand
waspCliNew appName template = return $ unwords ["wasp-cli", "new", appName, "-t", templateName]
  where
    templateName = case template of
      Basic -> "basic"
      Minimal -> "minimal"
      SaaS -> "saas"

waspCliCompletion :: ShellCommandBuilder context ShellCommand
waspCliCompletion = return "wasp-cli completion"

-- Wasp project commands

-- | Context for commands which are run from inside of a Wasp app project.
data WaspProjectContext = WaspProjectContext
  { waspProjectDir :: Path' Abs (Dir WaspProjectDir),
    waspProjectName :: String
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
waspCliDbMigrateDev :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbMigrateDev migrationName = do
  context <- ask
  let waspMigrationsDir = context.waspProjectDir </> dbMigrationsDirInWaspProjectDir
      waspOutMigrationsDir =
        context.waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir
          </> dbRootDirInProjectRootDir
          </> dbMigrationsDirInDbRootDir
   in return $
        unwords ["wasp-cli db migrate-dev --name", migrationName]
          ~&& replaceMigrationDatePrefix (fromAbsDir waspMigrationsDir)
          ~&& replaceMigrationDatePrefix (fromAbsDir waspOutMigrationsDir)
  where
    -- NOTE: We supress the `mv` error, because if we call `wasp db migrate-dev`
    -- when there is nothing to migrate, it succeeds but creates no files.
    replaceMigrationDatePrefix :: FilePath -> ShellCommand
    replaceMigrationDatePrefix migrationDirPath =
      unwords
        [ "mv",
          joinPath [migrationDirPath, "*" ++ migrationName],
          joinPath [migrationDirPath, "no-date-" ++ migrationName],
          "2>/dev/null || true"
        ]

waspCliDbSeed :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbSeed seedName = return $ "wasp-cli db seed " ++ seedName

waspCliDbReset :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbReset =
  return "wasp-cli db reset --force"

waspCliDbStudio :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbStudio = return "wasp-cli db studio"

waspCliInfo :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliInfo = return "wasp-cli info"

waspCliDeps :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDeps = return "wasp-cli deps"

waspCliDockerfile :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDockerfile = return "wasp-cli dockerfile"

waspCliStudio :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliStudio = return "wasp-cli studio"

-- NOTE: Fragile, assumes line numbers do not change.
setWaspDbToPSQL :: ShellCommandBuilder WaspProjectContext ShellCommand
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

appendToPrismaFile :: T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
appendToPrismaFile = appendToFile "schema.prisma"

createSeedFile :: String -> T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
createSeedFile fileName content = do
  context <- ask
  let seedDir = context.waspProjectDir </> seedsDirInWaspProjectDir
      seedFile = seedDir </> seedsFileInSeedsDir fileName

  createFile seedFile content

replaceMainWaspFile :: T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
replaceMainWaspFile content = do
  context <- ask
  let mainWaspFile = context.waspProjectDir </> mainWaspFileInWaspProjectDir

  createFile mainWaspFile content

-- | Builds and deletes the Docker image for a Wasp app.
-- Can be disabled via the @WASP_E2E_TESTS_SKIP_DOCKER@ environment variable.
buildAndRemoveWaspProjectDockerImage :: ShellCommandBuilder WaspProjectContext ShellCommand
buildAndRemoveWaspProjectDockerImage = do
  context <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ context.waspProjectName
   in return $
        "[ -z \"$WASP_E2E_TESTS_SKIP_DOCKER\" ]"
          ~? unwords ["cd", fromAbsDir (context.waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir)]
          ~&& unwords ["docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t", dockerImageTag, "."]
          ~&& unwords ["docker image rm", dockerImageTag]
          ~&& unwords ["cd", fromAbsDir context.waspProjectDir]

-- 'Test' specific commands

-- | Shell commands executed with this context are run from the 'FileSystem.TestCaseDir' directory.
data TestContext = TestContext
  { testCaseDir :: Path' Abs (Dir TestCaseDir),
    waspProjectContext :: WaspProjectContext
  }

inTestWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder TestContext ShellCommand
inTestWaspProjectDir waspProjectCommandBuilders = do
  context <- ask
  return $
    unwords ["cd", fromAbsDir context.waspProjectContext.waspProjectDir]
      ~&& foldr1 (~&&) (buildShellCommand context.waspProjectContext $ sequence waspProjectCommandBuilders)
      ~&& unwords ["cd", fromAbsDir context.testCaseDir]

createTestWaspProject :: WaspNewTemplate -> ShellCommandBuilder TestContext ShellCommand
createTestWaspProject template = do
  context <- ask
  waspCliNew context.waspProjectContext.waspProjectName template

-- 'SnapshotTest' specific commands

-- | Shell commands executed with this context are run from the 'FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  { snapshotDir :: Path' Abs (Dir SnapshotDir),
    waspProjectContext :: WaspProjectContext
  }

createSnapshotWaspProjectFromMinimalStarter :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspProjectFromMinimalStarter = do
  context <- ask
  waspCliNew context.waspProjectContext.waspProjectName Minimal

inSnapshotWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
inSnapshotWaspProjectDir waspProjectCommandBuilders = do
  context <- ask
  return $
    unwords ["cd", fromAbsDir context.waspProjectContext.waspProjectDir]
      ~&& foldr1 (~&&) (snapshotWaspProjectCommands context)
      ~&& ("cd " ++ fromAbsDir context.snapshotDir)
  where
    snapshotWaspProjectCommands :: SnapshotTestContext -> [ShellCommand]
    snapshotWaspProjectCommands snapshotTestContext =
      buildShellCommand snapshotTestContext.waspProjectContext $ sequence waspProjectCommandBuilders

copyContentsOfGitTrackedDirToSnapshotWaspProjectDir ::
  Path' (Rel GitRootDir) (Dir srcDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotWaspProjectDir srcDirFromGitRootDir = do
  context <- ask
  let snapshotWaspProjectDir = context.waspProjectContext.waspProjectDir

      listRelPathsOfGitTrackedFilesInSrcDir :: ShellCommand =
        "git -C " ++ fromRelDir gitRootFromSnapshotDir ++ " ls-files " ++ fromRelDir srcDirFromGitRootDir
      -- Remove the relative prefix from each path so that files get copied into the destination dir directly.
      -- e.g. `../../../../examples/todoApp/file.txt` -> `file.txt`
      stripSrcDirRelPrefixFromPaths :: ShellCommand =
        "sed 's#^" ++ fromRelDir srcDirFromGitRootDir ++ "##'"
      copyFromSrcDirToSnapshotWaspProjectDir :: ShellCommand =
        "rsync -a --files-from=- " ++ fromRelDir (gitRootFromSnapshotDir </> srcDirFromGitRootDir) ++ " " ++ fromAbsDir snapshotWaspProjectDir
   in return $
        unwords ["mkdir -p", fromAbsDir snapshotWaspProjectDir]
          ~&& listRelPathsOfGitTrackedFilesInSrcDir
          ~| stripSrcDirRelPrefixFromPaths
          ~| copyFromSrcDirToSnapshotWaspProjectDir
