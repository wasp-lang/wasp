-- | Actions shared by several e2e test definitions ("Tests/*"). Each function
-- is either a 'Command' value (to be run with 'runCommand' and friends) or a
-- 'TestAction' performing a native, platform-agnostic action.
--
-- Nothing here is privileged: a 'TestAction' is a plain @ReaderT ctx IO@, so a
-- test that needs a one-off action should write it inline rather than add it
-- here. This module is only for what more than one test uses.
module SharedActions
  ( -- * Running commands
    runCommand,
    runCommandGetResult,
    runCommandExpectingFailure,
    assertCommandSucceeded,
    assertCommandFailed,
    assertCommandSucceedsWithOutputContaining,
    assertCommandFailsWithOutputContaining,
    assertCommandStdoutFirstLineEquals,
    assertCommandStdoutTrimmedEquals,
    failCommand,

    -- * Wasp CLI commands
    waspCli,
    waspCliNew,
    waspCliVersion,
    waspCliTelemetry,
    waspCliCompile,
    waspCliStart,
    waspCliBuild,
    waspCliBuildStart,
    waspCliClean,
    waspCliDbSeed,
    waspCliDbReset,
    waspCliInfo,
    waspCliDeps,
    waspCliDockerfile,
    waspCliStudio,
    waspCliInstall,

    -- * Composite Wasp CLI actions
    waspCliDbMigrateDev,
    buildAndRemoveWaspProjectDockerImage,

    -- * File operations
    resolveInWorkingDir,
    writeToFile,
    appendToFile,
    copyFile,
    deleteFile,
    removeDirRecursively,

    -- * File system assertions
    assertDirExists,
    assertDirDoesNotExist,
    assertSymlinkExists,

    -- * Wasp project actions
    setWaspDbToPSQL,
    appendToPrismaFile,
    createSeedFile,
    replaceMainWaspTsFile,

    -- * 'Context.HasWaspProjectContext' actions
    createWaspProject,
    inWaspProjectDir,

    -- * 'Context.SnapshotTestContext' actions
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
  )
where

import Command
  ( Command,
    CommandResult (..),
    cmd,
    inAbsoluteDir,
    programFromEnvVar,
    showCommand,
  )
import qualified Command
import Context
  ( HasWaspProjectContext (getWaspProjectContext),
    HasWorkingDir (workingDir),
    SnapshotTestContext (..),
    WaspProjectContext (..),
  )
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks, withReaderT)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import FileSystem (GitRootDir, gitRootFromSnapshotDir, seedsDirInWaspProjectDir, seedsFileInSeedsDir)
import StrongPath (Abs, Dir, File, Path', Rel, fromAbsDir, fromAbsFile, fromRelDir, parent, (</>))
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import Test.Tasty.HUnit (assertFailure)
import TestAction (TestAction, logInfo)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)
import Wasp.Generator.DbGenerator.Common (dbMigrationsDirInDbRootDir, dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir, mainWaspTsFileInWaspProjectDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- Running commands

-- | Runs a command in the context's working directory and returns its result,
-- without asserting anything about it. The primitive the other command runners
-- are built from; use it directly for assertions this module doesn't cover.
runCommandGetResult :: (HasWorkingDir ctx) => Command -> TestAction ctx CommandResult
runCommandGetResult command = do
  commandWorkingDir <- asks workingDir
  liftIO $ Command.executeCommand commandWorkingDir command

-- | Runs a command in the context's working directory, asserting it succeeds.
runCommand :: (HasWorkingDir ctx) => Command -> TestAction ctx ()
runCommand command = do
  result <- runCommandGetResult command
  liftIO $ assertCommandSucceeded command result

-- | Runs a command, asserting it fails (exits with a non-zero code).
runCommandExpectingFailure :: (HasWorkingDir ctx) => Command -> TestAction ctx ()
runCommandExpectingFailure command = do
  result <- runCommandGetResult command
  liftIO $ assertCommandFailed command result

-- | Asserts that the command succeeds and that its output (stdout and stderr
-- combined) contains the given text.
assertCommandSucceedsWithOutputContaining :: (HasWorkingDir ctx) => Command -> String -> TestAction ctx ()
assertCommandSucceedsWithOutputContaining command expectedOutputPart = do
  result <- runCommandGetResult command
  liftIO $ do
    assertCommandSucceeded command result
    assertOutputContains command result expectedOutputPart

-- | Asserts that the command fails and that its output (stdout and stderr
-- combined) contains the given text.
assertCommandFailsWithOutputContaining :: (HasWorkingDir ctx) => Command -> String -> TestAction ctx ()
assertCommandFailsWithOutputContaining command expectedOutputPart = do
  result <- runCommandGetResult command
  liftIO $ do
    assertCommandFailed command result
    assertOutputContains command result expectedOutputPart

assertCommandStdoutFirstLineEquals :: (HasWorkingDir ctx) => Command -> String -> TestAction ctx ()
assertCommandStdoutFirstLineEquals command expectedFirstLine =
  assertCommandStdoutMatches
    ("first line of stdout to equal " ++ show expectedFirstLine)
    (\stdoutText -> T.strip (firstLine stdoutText) == T.pack expectedFirstLine)
    command
  where
    firstLine stdoutText = case T.lines stdoutText of
      (line : _) -> line
      [] -> ""

assertCommandStdoutTrimmedEquals :: (HasWorkingDir ctx) => Command -> String -> TestAction ctx ()
assertCommandStdoutTrimmedEquals command expectedStdout =
  assertCommandStdoutMatches
    ("trimmed stdout to equal " ++ show expectedStdout)
    (\stdoutText -> T.strip stdoutText == T.pack expectedStdout)
    command

-- | Asserts the command succeeds and that its stdout satisfies the predicate.
assertCommandStdoutMatches ::
  (HasWorkingDir ctx) => String -> (T.Text -> Bool) -> Command -> TestAction ctx ()
assertCommandStdoutMatches expectationDescription matches command = do
  result <- runCommandGetResult command
  liftIO $ do
    assertCommandSucceeded command result
    unless (matches result.stdoutText) $
      failCommand command result $
        "Expected " ++ expectationDescription ++ ", but stdout was: " ++ show result.stdoutText

-- | Fails the test, reporting the command, why it failed, and its output.
-- The output is the main thing you need to debug an e2e failure, so it is
-- always included.
failCommand :: Command -> CommandResult -> String -> IO a
failCommand command result reason =
  assertFailure $
    unlines
      [ "Command: " ++ showCommand command,
        reason,
        "=== Command output ===",
        T.unpack result.combinedOutput
      ]

assertOutputContains :: Command -> CommandResult -> String -> IO ()
assertOutputContains command result expectedOutputPart =
  unless (T.pack expectedOutputPart `T.isInfixOf` result.combinedOutput) $
    failCommand command result $
      "Expected the command output to contain " ++ show expectedOutputPart

-- | Fails the test unless the command exited successfully.
assertCommandSucceeded :: Command -> CommandResult -> IO ()
assertCommandSucceeded command result = case result.exitCode of
  ExitSuccess -> return ()
  ExitFailure code ->
    failCommand command result $
      "Expected the command to succeed, but it exited with code " ++ show code

-- | Fails the test unless the command exited with a failure (non-zero) code.
assertCommandFailed :: Command -> CommandResult -> IO ()
assertCommandFailed command result = case result.exitCode of
  ExitFailure _ -> return ()
  ExitSuccess -> failCommand command result "Expected the command to fail, but it succeeded"

-- Wasp CLI commands

-- | The dev Wasp CLI is run through the @WASP_CLI_CMD@ executable (a @cabal run@
-- wrapper set by @./run@ and 'Main.ensureE2eTestsEnvironment'), falling back to
-- @wasp-cli@ on @PATH@ when the variable is unset.
waspCli :: [String] -> Command
waspCli = programFromEnvVar "WASP_CLI_CMD" . cmd "wasp-cli"

waspCliNew :: String -> StarterTemplate -> Command
waspCliNew appName starterTemplate = waspCli ["new", appName, "-t", show starterTemplate]

waspCliVersion :: Command
waspCliVersion = waspCli ["version"]

waspCliTelemetry :: Command
waspCliTelemetry = waspCli ["telemetry"]

waspCliCompile :: Command
waspCliCompile = waspCli ["compile"]

waspCliStart :: Command
waspCliStart = waspCli ["start"]

waspCliBuild :: Command
waspCliBuild = waspCli ["build"]

waspCliBuildStart :: [String] -> Command
waspCliBuildStart args = waspCli ("build" : "start" : args)

waspCliClean :: Command
waspCliClean = waspCli ["clean"]

waspCliDbSeed :: [String] -> Command
waspCliDbSeed seedArgs = waspCli ("db" : "seed" : seedArgs)

waspCliDbReset :: Command
waspCliDbReset = waspCli ["db", "reset", "--force"]

waspCliInfo :: Command
waspCliInfo = waspCli ["info"]

waspCliDeps :: Command
waspCliDeps = waspCli ["deps"]

waspCliDockerfile :: Command
waspCliDockerfile = waspCli ["dockerfile"]

waspCliStudio :: Command
waspCliStudio = waspCli ["studio"]

waspCliInstall :: Command
waspCliInstall = waspCli ["install"]

-- Composite Wasp CLI actions

-- | We make the migration name deterministic by renaming the migration dir to
-- @no-date-<migrationName>@, instead of the usual @<date>_<migrationName>@.
-- This is important for snapshot testing as we don't want a different migration name each time.
-- Caveat: this does mean that we can't have two migrations with the same name in a project.
waspCliDbMigrateDev :: String -> TestAction WaspProjectContext ()
waspCliDbMigrateDev migrationName = do
  context <- ask
  runCommand $ waspCli ["db", "migrate-dev", "--name", migrationName]
  let waspMigrationsDir = context.waspProjectDir </> dbMigrationsDirInWaspProjectDir
      waspOutMigrationsDir =
        context.waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir
          </> dbRootDirInGeneratedAppDir
          </> dbMigrationsDirInDbRootDir
  liftIO $ do
    removeMigrationDirDatePrefix waspMigrationsDir
    removeMigrationDirDatePrefix waspOutMigrationsDir
  where
    -- NOTE: When there is nothing to migrate, `wasp db migrate-dev` succeeds
    -- but creates no migration dir, in which case this is a no-op.
    removeMigrationDirDatePrefix :: Path' Abs (Dir d) -> IO ()
    removeMigrationDirDatePrefix migrationsDir = do
      migrationsDirExists <- SD.doesDirectoryExist (fromAbsDir migrationsDir)
      when migrationsDirExists $ do
        entryNames <- SD.listDirectory (fromAbsDir migrationsDir)
        let migrationDirNames = filter isUnnormalizedMigrationDirName entryNames
        forM_ migrationDirNames $ \migrationDirName ->
          SD.renameDirectory
            (fromAbsDir migrationsDir FP.</> migrationDirName)
            (fromAbsDir migrationsDir FP.</> ("no-date-" ++ migrationName))

    -- A migration dir for this migration (its name ends with the migration name)
    -- that we haven't normalized yet (i.e. it still has its date prefix).
    isUnnormalizedMigrationDirName entryName =
      migrationName `isSuffixOf` entryName && entryName /= "no-date-" ++ migrationName

-- | Builds and deletes the Docker image for a Wasp app.
-- Can be disabled via the @WASP_E2E_TESTS_SKIP_DOCKER@ environment variable.
buildAndRemoveWaspProjectDockerImage :: TestAction WaspProjectContext ()
buildAndRemoveWaspProjectDockerImage =
  unlessEnvVarSet "WASP_E2E_TESTS_SKIP_DOCKER" $ do
    context <- ask
    let dockerImageTag = "waspc-e2e-tests-" ++ context.waspProjectName
        generatedAppDir = context.waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedAppDirInDotWaspDir
    runCommand $
      inAbsoluteDir generatedAppDir $
        cmd "docker" ["build", "--build-arg", "BUILDKIT_DOCKERFILE_CHECK=error=true", "-t", dockerImageTag, "."]
    runCommand $ cmd "docker" ["image", "rm", dockerImageTag]

-- | Runs the given action unless the environment variable is set to a non-empty
-- value, reporting the skip so it doesn't look like the action ran.
unlessEnvVarSet :: String -> TestAction ctx () -> TestAction ctx ()
unlessEnvVarSet envVarName action = do
  maybeValue <- liftIO $ lookupEnv envVarName
  case maybeValue of
    Just value
      | not (null value) ->
          logInfo $ "Skipping: env var " ++ envVarName ++ " is set."
    _ -> action

-- File operations

-- | Resolves a path relative to the context's working directory.
resolveInWorkingDir :: (HasWorkingDir ctx) => FilePath -> TestAction ctx FilePath
resolveInWorkingDir path = asks $ \context -> fromAbsDir (workingDir context) FP.</> path

-- | Writes the text to the file (as UTF-8 bytes), creating parent directories
-- if needed and overwriting the file if it already exists.
writeToFile :: Path' Abs (File f) -> T.Text -> TestAction ctx ()
writeToFile file fileContent = liftIO $ do
  SD.createDirectoryIfMissing True (fromAbsDir $ parent file)
  BS.writeFile (fromAbsFile file) (TE.encodeUtf8 fileContent)

-- | Appends the text and a trailing newline to the file (relative to the
-- working directory), creating the file if it does not exist.
appendToFile :: (HasWorkingDir ctx) => FilePath -> T.Text -> TestAction ctx ()
appendToFile fileName content = do
  filePath <- resolveInWorkingDir fileName
  liftIO $ BS.appendFile filePath (TE.encodeUtf8 $ content <> "\n")

-- | Replaces the given (1-based) line of the file (relative to the working directory).
replaceLineInFile :: (HasWorkingDir ctx) => FilePath -> Int -> String -> TestAction ctx ()
replaceLineInFile fileName lineNumber newLine = do
  filePath <- resolveInWorkingDir fileName
  liftIO $ do
    fileLines <- T.lines . TE.decodeUtf8 <$> BS.readFile filePath
    let updatedFileLines = case splitAt (lineNumber - 1) fileLines of
          (linesBefore, _replacedLine : linesAfter) -> linesBefore ++ [T.pack newLine] ++ linesAfter
          _ -> fileLines
    BS.writeFile filePath (TE.encodeUtf8 $ T.unlines updatedFileLines)

-- | Copies a file to another path, both relative to the working directory.
copyFile :: (HasWorkingDir ctx) => FilePath -> FilePath -> TestAction ctx ()
copyFile srcFileName dstFileName = do
  srcFilePath <- resolveInWorkingDir srcFileName
  dstFilePath <- resolveInWorkingDir dstFileName
  liftIO $ SD.copyFile srcFilePath dstFilePath

-- | Deletes a file (relative to the working directory), failing if it does not exist.
deleteFile :: (HasWorkingDir ctx) => FilePath -> TestAction ctx ()
deleteFile fileName = resolveInWorkingDir fileName >>= liftIO . SD.removeFile

-- | Removes a directory (relative to the working directory) and its contents,
-- if it exists.
removeDirRecursively :: (HasWorkingDir ctx) => FilePath -> TestAction ctx ()
removeDirRecursively dirName = resolveInWorkingDir dirName >>= liftIO . SD.removePathForcibly

-- File system assertions

assertDirExists :: (HasWorkingDir ctx) => FilePath -> TestAction ctx ()
assertDirExists dirName = do
  dirPath <- resolveInWorkingDir dirName
  liftIO $ do
    dirExists <- SD.doesDirectoryExist dirPath
    unless dirExists $ assertFailure $ "Expected dir to exist: " ++ dirPath

assertDirDoesNotExist :: (HasWorkingDir ctx) => FilePath -> TestAction ctx ()
assertDirDoesNotExist dirName = do
  dirPath <- resolveInWorkingDir dirName
  liftIO $ do
    dirExists <- SD.doesDirectoryExist dirPath
    when dirExists $ assertFailure $ "Expected dir not to exist: " ++ dirPath

assertSymlinkExists :: (HasWorkingDir ctx) => FilePath -> TestAction ctx ()
assertSymlinkExists path = do
  resolvedPath <- resolveInWorkingDir path
  liftIO $ do
    pathExists <- SD.doesPathExist resolvedPath
    isSymlink <- if pathExists then SD.pathIsSymbolicLink resolvedPath else return False
    unless isSymlink $ assertFailure $ "Expected path to be a symlink: " ++ resolvedPath

-- Wasp project actions

-- NOTE: Fragile, assumes line numbers do not change.
setWaspDbToPSQL :: TestAction WaspProjectContext ()
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

appendToPrismaFile :: T.Text -> TestAction WaspProjectContext ()
appendToPrismaFile = appendToFile "schema.prisma"

createSeedFile :: String -> T.Text -> TestAction WaspProjectContext ()
createSeedFile fileName content = do
  context <- ask
  let seedFile = context.waspProjectDir </> seedsDirInWaspProjectDir </> seedsFileInSeedsDir fileName
  writeToFile seedFile content

replaceMainWaspTsFile :: T.Text -> TestAction WaspProjectContext ()
replaceMainWaspTsFile content = do
  context <- ask
  writeToFile (context.waspProjectDir </> mainWaspTsFileInWaspProjectDir) content

-- 'Context.HasWaspProjectContext' actions

createWaspProject :: (HasWaspProjectContext ctx, HasWorkingDir ctx) => StarterTemplate -> TestAction ctx ()
createWaspProject template = do
  context <- ask
  runCommand $ waspCliNew (getWaspProjectContext context).waspProjectName template

-- | Runs actions that expect a 'WaspProjectContext', e.g. from a test's
-- 'Context.TestContext' or 'Context.SnapshotTestContext'.
inWaspProjectDir :: (HasWaspProjectContext ctx) => TestAction WaspProjectContext a -> TestAction ctx a
inWaspProjectDir = withReaderT getWaspProjectContext

-- 'Context.SnapshotTestContext' actions

copyContentsOfGitTrackedDirToSnapshotWaspProjectDir ::
  Path' (Rel GitRootDir) (Dir srcDir) ->
  TestAction SnapshotTestContext ()
copyContentsOfGitTrackedDirToSnapshotWaspProjectDir srcDirFromGitRootDir = do
  let gitLsFiles =
        cmd "git" ["-C", fromRelDir gitRootFromSnapshotDir, "ls-files", fromRelDir srcDirFromGitRootDir]
  gitLsFilesResult <- runCommandGetResult gitLsFiles
  liftIO $ assertCommandSucceeded gitLsFiles gitLsFilesResult

  context <- ask
  let snapshotWaspProjectDirPath = fromAbsDir context.waspProjectContext.waspProjectDir
      gitRootDirPath = fromAbsDir context.snapshotDir FP.</> fromRelDir gitRootFromSnapshotDir
      trackedFilePathsFromGitRoot = lines $ T.unpack gitLsFilesResult.stdoutText
  liftIO $ do
    SD.createDirectoryIfMissing True snapshotWaspProjectDirPath
    forM_ trackedFilePathsFromGitRoot $ \filePathFromGitRoot -> do
      let filePathInSrcDir = FP.makeRelative (fromRelDir srcDirFromGitRootDir) filePathFromGitRoot
          dstFilePath = snapshotWaspProjectDirPath FP.</> filePathInSrcDir
      SD.createDirectoryIfMissing True (FP.takeDirectory dstFilePath)
      SD.copyFile (gitRootDirPath FP.</> filePathFromGitRoot) dstFilePath
