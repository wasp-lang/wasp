-- | The DSL that e2e test definitions ("Tests/*") are written in.
-- Each function is either a 'Command' value (to be run with 'runCommand' and
-- friends) or a 'Step' executing a native, platform-agnostic action.
module Steps
  ( -- * Running commands
    runCommand,
    runCommandExpectingFailure,
    assertCommandSucceedsWithOutputContaining,
    assertCommandFailsWithOutputContaining,
    assertCommandStdoutFirstLineEquals,
    assertCommandStdoutTrimmedEquals,

    -- * Wasp CLI commands
    waspCli,
    waspCliNew,
    waspCliNewInteractive,
    waspCliCompletion,
    waspCliCompletionList,
    waspCliVersion,
    waspCliTelemetry,
    waspCliNews,
    waspCliCompile,
    waspCliStart,
    waspCliStartDb,
    waspCliTestClient,
    waspCliBuild,
    waspCliBuildStart,
    waspCliClean,
    waspCliDbStart,
    waspCliDbSeed,
    waspCliDbReset,
    waspCliDbStudio,
    waspCliInfo,
    waspCliDeps,
    waspCliDeploy,
    waspCliDockerfile,
    waspCliStudio,
    waspCliInstall,

    -- * Composite Wasp CLI steps
    waspCliDbMigrateDev,
    buildAndRemoveWaspProjectDockerImage,
    unlessEnvVarSet,

    -- * File operations
    writeToFile,
    appendToFile,
    replaceLineInFile,
    copyFile,
    deleteFile,
    removeDirRecursively,

    -- * File system assertions
    assertDirExists,
    assertDirDoesNotExist,
    assertFileExists,
    assertSymlinkExists,
    assertDirHasSubdirWithNameContaining,

    -- * Wasp project steps
    setWaspDbToPSQL,
    appendToPrismaFile,
    createSeedFile,
    replaceMainWaspTsFile,

    -- * 'Context.TestContext' steps
    createTestWaspProject,
    inTestWaspProjectDir,

    -- * 'Context.SnapshotTestContext' steps
    createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
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
    withStdin,
  )
import qualified Command
import Context
  ( HasWorkingDir (workingDir),
    SnapshotTestContext (..),
    TestContext (..),
    WaspProjectContext (..),
  )
import Control.Monad (filterM, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import FileSystem (GitRootDir, gitRootFromSnapshotDir, seedsDirInWaspProjectDir, seedsFileInSeedsDir)
import Step (Step, askStepContext, failStep, liftStepIO, makeStep, withInnerContext)
import StrongPath (Abs, Dir, File, Path', Rel, fromAbsDir, fromAbsFile, fromRelDir, parent, (</>))
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)
import Wasp.Generator.DbGenerator.Common (dbMigrationsDirInDbRootDir, dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir, mainWaspTsFileInWaspProjectDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- Running commands

-- | Runs a command in the step context's working directory, asserting it succeeds.
runCommand :: (HasWorkingDir ctx) => Command -> Step ctx ()
runCommand command = do
  result <- runCommandGetResult command
  liftStepIO $ case result.exitCode of
    ExitSuccess -> return ()
    ExitFailure code ->
      failStep (runDescription command) ("command exited with code " ++ show code)

-- | Runs a command, asserting it fails (exits with a non-zero code).
runCommandExpectingFailure :: (HasWorkingDir ctx) => Command -> Step ctx ()
runCommandExpectingFailure command = do
  result <- runCommandGetResultDescribed description command
  liftStepIO $ case result.exitCode of
    ExitFailure _ -> return ()
    ExitSuccess -> failStep description "command succeeded, but it was expected to fail"
  where
    description = "run expecting failure: " ++ showCommand command

-- | Asserts that the command succeeds and that its output (stdout and stderr
-- combined) contains the given text.
assertCommandSucceedsWithOutputContaining :: (HasWorkingDir ctx) => Command -> String -> Step ctx ()
assertCommandSucceedsWithOutputContaining command expectedOutputPart = do
  result <- runCommandGetResultDescribed description command
  liftStepIO $ do
    case result.exitCode of
      ExitSuccess -> return ()
      ExitFailure code -> failStep description ("command exited with code " ++ show code)
    assertOutputContains description result expectedOutputPart
  where
    description = showCommand command ++ " (expecting output to contain " ++ show expectedOutputPart ++ ")"

-- | Asserts that the command fails and that its output (stdout and stderr
-- combined) contains the given text.
assertCommandFailsWithOutputContaining :: (HasWorkingDir ctx) => Command -> String -> Step ctx ()
assertCommandFailsWithOutputContaining command expectedOutputPart = do
  result <- runCommandGetResultDescribed description command
  liftStepIO $ do
    case result.exitCode of
      ExitFailure _ -> return ()
      ExitSuccess -> failStep description "command succeeded, but it was expected to fail"
    assertOutputContains description result expectedOutputPart
  where
    description = "run expecting failure: " ++ showCommand command ++ " (expecting output to contain " ++ show expectedOutputPart ++ ")"

assertCommandStdoutFirstLineEquals :: (HasWorkingDir ctx) => Command -> String -> Step ctx ()
assertCommandStdoutFirstLineEquals command expectedFirstLine =
  assertCommandStdoutMatches
    ("first line of stdout equals " ++ show expectedFirstLine)
    (\stdoutText -> T.strip (firstLine stdoutText) == T.pack expectedFirstLine)
    command
  where
    firstLine stdoutText = case T.lines stdoutText of
      [] -> ""
      (line : _) -> line

assertCommandStdoutTrimmedEquals :: (HasWorkingDir ctx) => Command -> String -> Step ctx ()
assertCommandStdoutTrimmedEquals command expectedStdout =
  assertCommandStdoutMatches
    ("trimmed stdout equals " ++ show expectedStdout)
    (\stdoutText -> T.strip stdoutText == T.pack expectedStdout)
    command

assertCommandStdoutMatches :: (HasWorkingDir ctx) => String -> (T.Text -> Bool) -> Command -> Step ctx ()
assertCommandStdoutMatches expectationDescription matches command = do
  result <- runCommandGetResultDescribed description command
  liftStepIO $ do
    case result.exitCode of
      ExitSuccess -> return ()
      ExitFailure code -> failStep description ("command exited with code " ++ show code)
    unless (matches result.stdoutText) $
      failStep description ("stdout was: " ++ show result.stdoutText)
  where
    description = showCommand command ++ " (expecting " ++ expectationDescription ++ ")"

runCommandGetResult :: (HasWorkingDir ctx) => Command -> Step ctx CommandResult
runCommandGetResult command = runCommandGetResultDescribed (runDescription command) command

runCommandGetResultDescribed :: (HasWorkingDir ctx) => String -> Command -> Step ctx CommandResult
runCommandGetResultDescribed description command =
  makeStep description $ \logger context ->
    Command.executeCommand logger (workingDir context) command

runDescription :: Command -> String
runDescription command = "run: " ++ showCommand command

assertOutputContains :: String -> CommandResult -> String -> IO ()
assertOutputContains stepDescription result expectedOutputPart =
  unless (T.pack expectedOutputPart `T.isInfixOf` result.combinedOutput) $
    failStep stepDescription ("command output does not contain " ++ show expectedOutputPart)

-- Wasp CLI commands

-- | The dev Wasp CLI is run through the @WASP_CLI_CMD@ executable (a @cabal run@
-- wrapper set by @./run@ and 'Main.ensureE2eTestsEnvironment'), falling back to
-- @wasp-cli@ on @PATH@ when the variable is unset.
waspCli :: [String] -> Command
waspCli = programFromEnvVar "WASP_CLI_CMD" . cmd "wasp-cli"

waspCliNew :: String -> StarterTemplate -> Command
waspCliNew appName starterTemplate = waspCli ["new", appName, "-t", show starterTemplate]

waspCliNewInteractive :: String -> StarterTemplate -> Command
waspCliNewInteractive appName starterTemplate =
  withStdin (T.pack $ appName ++ "\n" ++ show starterTemplate ++ "\n") (waspCli ["new"])

waspCliCompletion :: Command
waspCliCompletion = waspCli ["completion"]

waspCliCompletionList :: Command
waspCliCompletionList = waspCli ["completion:list"]

waspCliVersion :: Command
waspCliVersion = waspCli ["version"]

waspCliTelemetry :: Command
waspCliTelemetry = waspCli ["telemetry"]

waspCliNews :: Command
waspCliNews = waspCli ["news"]

waspCliCompile :: Command
waspCliCompile = waspCli ["compile"]

waspCliStart :: Command
waspCliStart = waspCli ["start"]

waspCliStartDb :: Command
waspCliStartDb = waspCli ["start", "db"]

waspCliTestClient :: [String] -> Command
waspCliTestClient testArgs = waspCli ("test" : "client" : testArgs)

waspCliBuild :: Command
waspCliBuild = waspCli ["build"]

waspCliBuildStart :: [String] -> Command
waspCliBuildStart args = waspCli ("build" : "start" : args)

waspCliClean :: Command
waspCliClean = waspCli ["clean"]

waspCliDbStart :: Command
waspCliDbStart = waspCli ["db", "start"]

waspCliDbSeed :: [String] -> Command
waspCliDbSeed seedArgs = waspCli ("db" : "seed" : seedArgs)

waspCliDbReset :: Command
waspCliDbReset = waspCli ["db", "reset", "--force"]

waspCliDbStudio :: Command
waspCliDbStudio = waspCli ["db", "studio"]

waspCliInfo :: Command
waspCliInfo = waspCli ["info"]

waspCliDeps :: Command
waspCliDeps = waspCli ["deps"]

waspCliDeploy :: [String] -> Command
waspCliDeploy deployArgs = waspCli ("deploy" : deployArgs)

waspCliDockerfile :: Command
waspCliDockerfile = waspCli ["dockerfile"]

waspCliStudio :: Command
waspCliStudio = waspCli ["studio"]

waspCliInstall :: Command
waspCliInstall = waspCli ["install"]

-- Composite Wasp CLI steps

-- | We make the migration name deterministic by renaming the migration dir to
-- @no-date-<migrationName>@, instead of the usual @<date>_<migrationName>@.
-- This is important for snapshot testing as we don't want a different migration name each time.
-- Caveat: this does mean that we can't have two migrations with the same name in a project.
waspCliDbMigrateDev :: String -> Step WaspProjectContext ()
waspCliDbMigrateDev migrationName = do
  context <- askStepContext
  runCommand $ waspCli ["db", "migrate-dev", "--name", migrationName]
  let waspMigrationsDir = context.waspProjectDir </> dbMigrationsDirInWaspProjectDir
      waspOutMigrationsDir =
        context.waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir
          </> dbRootDirInGeneratedAppDir
          </> dbMigrationsDirInDbRootDir
  removeMigrationDirDatePrefix waspMigrationsDir
  removeMigrationDirDatePrefix waspOutMigrationsDir
  where
    -- NOTE: When there is nothing to migrate, `wasp db migrate-dev` succeeds
    -- but creates no migration dir, in which case this is a no-op.
    removeMigrationDirDatePrefix :: Path' Abs (Dir d) -> Step WaspProjectContext ()
    removeMigrationDirDatePrefix migrationsDir =
      makeStep ("normalize name of migration " ++ show migrationName ++ " in: " ++ fromAbsDir migrationsDir) $ \_ _ -> do
        migrationsDirExists <- SD.doesDirectoryExist (fromAbsDir migrationsDir)
        when migrationsDirExists $ do
          entryNames <- SD.listDirectory (fromAbsDir migrationsDir)
          let migrationDirNames = filter isMigrationDirNameWithDatePrefix entryNames
          forM_ migrationDirNames $ \migrationDirName ->
            SD.renameDirectory
              (fromAbsDir migrationsDir FP.</> migrationDirName)
              (fromAbsDir migrationsDir FP.</> ("no-date-" ++ migrationName))

    isMigrationDirNameWithDatePrefix entryName =
      migrationName `isSuffixOf` entryName && entryName /= "no-date-" ++ migrationName

-- | Builds and deletes the Docker image for a Wasp app.
-- Can be disabled via the @WASP_E2E_TESTS_SKIP_DOCKER@ environment variable.
buildAndRemoveWaspProjectDockerImage :: Step WaspProjectContext ()
buildAndRemoveWaspProjectDockerImage =
  unlessEnvVarSet "WASP_E2E_TESTS_SKIP_DOCKER" $ do
    context <- askStepContext
    let dockerImageTag = "waspc-e2e-tests-" ++ context.waspProjectName
        generatedAppDir = context.waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedAppDirInDotWaspDir
    runCommand $
      inAbsoluteDir generatedAppDir $
        cmd "docker" ["build", "--build-arg", "BUILDKIT_DOCKERFILE_CHECK=error=true", "-t", dockerImageTag, "."]
    runCommand $ cmd "docker" ["image", "rm", dockerImageTag]

-- | Runs the given step unless the environment variable is set to a non-empty value.
unlessEnvVarSet :: String -> Step ctx () -> Step ctx ()
unlessEnvVarSet envVarName step = do
  maybeValue <- liftStepIO $ lookupEnv envVarName
  case maybeValue of
    Just value
      | not (null value) ->
          makeStep ("skipped step(s): env var " ++ envVarName ++ " is set") $ \_ _ -> return ()
    _ -> step

-- File operations

-- | Writes the text to the file (as UTF-8 bytes), creating parent directories
-- if needed and overwriting the file if it already exists.
writeToFile :: Path' Abs (File f) -> T.Text -> Step ctx ()
writeToFile file fileContent =
  makeStep ("write file: " ++ fromAbsFile file) $ \_ _ -> do
    SD.createDirectoryIfMissing True (fromAbsDir $ parent file)
    BS.writeFile (fromAbsFile file) (TE.encodeUtf8 fileContent)

-- | Appends the text and a trailing newline to the file (relative to the
-- working directory), creating the file if it does not exist.
appendToFile :: (HasWorkingDir ctx) => FilePath -> T.Text -> Step ctx ()
appendToFile fileName content =
  makeStep ("append to file: " ++ fileName) $ \_ context ->
    BS.appendFile (resolveInWorkingDir context fileName) (TE.encodeUtf8 $ content <> "\n")

-- | Replaces the given (1-based) line of the file (relative to the working directory).
replaceLineInFile :: (HasWorkingDir ctx) => FilePath -> Int -> String -> Step ctx ()
replaceLineInFile fileName lineNumber newLine =
  makeStep ("replace line " ++ show lineNumber ++ " in file: " ++ fileName) $ \_ context -> do
    let filePath = resolveInWorkingDir context fileName
    fileLines <- T.lines . TE.decodeUtf8 <$> BS.readFile filePath
    let updatedFileLines = case splitAt (lineNumber - 1) fileLines of
          (linesBefore, _replacedLine : linesAfter) -> linesBefore ++ [T.pack newLine] ++ linesAfter
          _ -> fileLines
    BS.writeFile filePath (TE.encodeUtf8 $ T.unlines updatedFileLines)

-- | Copies a file to another path, both relative to the working directory.
copyFile :: (HasWorkingDir ctx) => FilePath -> FilePath -> Step ctx ()
copyFile srcFileName dstFileName =
  makeStep ("copy file: " ++ srcFileName ++ " -> " ++ dstFileName) $ \_ context ->
    SD.copyFile (resolveInWorkingDir context srcFileName) (resolveInWorkingDir context dstFileName)

-- | Deletes a file (relative to the working directory), failing if it does not exist.
deleteFile :: (HasWorkingDir ctx) => FilePath -> Step ctx ()
deleteFile fileName =
  makeStep ("delete file: " ++ fileName) $ \_ context ->
    SD.removeFile (resolveInWorkingDir context fileName)

-- | Removes a directory (relative to the working directory) and its contents,
-- if it exists.
removeDirRecursively :: (HasWorkingDir ctx) => FilePath -> Step ctx ()
removeDirRecursively dirName =
  makeStep ("remove dir: " ++ dirName) $ \_ context ->
    SD.removePathForcibly (resolveInWorkingDir context dirName)

-- File system assertions

assertDirExists :: (HasWorkingDir ctx) => FilePath -> Step ctx ()
assertDirExists dirName =
  makeStep description $ \_ context -> do
    dirExists <- SD.doesDirectoryExist (resolveInWorkingDir context dirName)
    unless dirExists $ failStep description "directory does not exist"
  where
    description = "assert dir exists: " ++ dirName

assertDirDoesNotExist :: (HasWorkingDir ctx) => FilePath -> Step ctx ()
assertDirDoesNotExist dirName =
  makeStep description $ \_ context -> do
    dirExists <- SD.doesDirectoryExist (resolveInWorkingDir context dirName)
    when dirExists $ failStep description "directory exists"
  where
    description = "assert dir does not exist: " ++ dirName

assertFileExists :: (HasWorkingDir ctx) => FilePath -> Step ctx ()
assertFileExists fileName =
  makeStep description $ \_ context -> do
    fileExists <- SD.doesFileExist (resolveInWorkingDir context fileName)
    unless fileExists $ failStep description "file does not exist"
  where
    description = "assert file exists: " ++ fileName

assertSymlinkExists :: (HasWorkingDir ctx) => FilePath -> Step ctx ()
assertSymlinkExists path =
  makeStep description $ \_ context -> do
    let resolvedPath = resolveInWorkingDir context path
    pathExists <- SD.doesPathExist resolvedPath
    isSymlink <- if pathExists then SD.pathIsSymbolicLink resolvedPath else return False
    unless isSymlink $ failStep description "path is not a symlink"
  where
    description = "assert symlink exists: " ++ path

-- | Asserts that the directory has an (immediate) subdirectory whose name
-- contains the given text.
assertDirHasSubdirWithNameContaining :: Path' Abs (Dir d) -> String -> Step ctx ()
assertDirHasSubdirWithNameContaining dir expectedNamePart =
  makeStep description $ \_ _ -> do
    entryNames <- SD.listDirectory (fromAbsDir dir)
    subdirNames <-
      filterM
        (SD.doesDirectoryExist . (fromAbsDir dir FP.</>))
        (filter (expectedNamePart `isInfixOf`) entryNames)
    when (null subdirNames) $ failStep description "no such subdirectory"
  where
    description = "assert dir " ++ fromAbsDir dir ++ " has subdir with name containing " ++ show expectedNamePart

-- Wasp project steps

-- NOTE: Fragile, assumes line numbers do not change.
setWaspDbToPSQL :: Step WaspProjectContext ()
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

appendToPrismaFile :: T.Text -> Step WaspProjectContext ()
appendToPrismaFile = appendToFile "schema.prisma"

createSeedFile :: String -> T.Text -> Step WaspProjectContext ()
createSeedFile fileName content = do
  context <- askStepContext
  let seedFile = context.waspProjectDir </> seedsDirInWaspProjectDir </> seedsFileInSeedsDir fileName
  writeToFile seedFile content

replaceMainWaspTsFile :: T.Text -> Step WaspProjectContext ()
replaceMainWaspTsFile content = do
  context <- askStepContext
  writeToFile (context.waspProjectDir </> mainWaspTsFileInWaspProjectDir) content

-- 'Context.TestContext' steps

createTestWaspProject :: StarterTemplate -> Step TestContext ()
createTestWaspProject template = do
  context <- askStepContext
  runCommand $ waspCliNew context.waspProjectContext.waspProjectName template

inTestWaspProjectDir :: Step WaspProjectContext () -> Step TestContext ()
inTestWaspProjectDir steps = do
  context <- askStepContext
  withInnerContext context.waspProjectContext steps

-- 'Context.SnapshotTestContext' steps

createSnapshotWaspProjectFromMinimalStarter :: Step SnapshotTestContext ()
createSnapshotWaspProjectFromMinimalStarter = do
  context <- askStepContext
  runCommand $ waspCliNew context.waspProjectContext.waspProjectName minimalStarterTemplate

inSnapshotWaspProjectDir :: Step WaspProjectContext () -> Step SnapshotTestContext ()
inSnapshotWaspProjectDir steps = do
  context <- askStepContext
  withInnerContext context.waspProjectContext steps

copyContentsOfGitTrackedDirToSnapshotWaspProjectDir ::
  Path' (Rel GitRootDir) (Dir srcDir) ->
  Step SnapshotTestContext ()
copyContentsOfGitTrackedDirToSnapshotWaspProjectDir srcDirFromGitRootDir = do
  gitLsFilesResult <-
    runCommandGetResult $
      cmd "git" ["-C", fromRelDir gitRootFromSnapshotDir, "ls-files", fromRelDir srcDirFromGitRootDir]
  makeStep description $ \_ context -> do
    case gitLsFilesResult.exitCode of
      ExitSuccess -> return ()
      ExitFailure code -> failStep description ("git ls-files exited with code " ++ show code)
    let snapshotWaspProjectDirPath = fromAbsDir context.waspProjectContext.waspProjectDir
        gitRootDirPath = fromAbsDir context.snapshotDir FP.</> fromRelDir gitRootFromSnapshotDir
        trackedFilePathsFromGitRoot = lines $ T.unpack gitLsFilesResult.stdoutText
    SD.createDirectoryIfMissing True snapshotWaspProjectDirPath
    forM_ trackedFilePathsFromGitRoot $ \filePathFromGitRoot -> do
      let filePathInSrcDir = FP.makeRelative (fromRelDir srcDirFromGitRootDir) filePathFromGitRoot
          dstFilePath = snapshotWaspProjectDirPath FP.</> filePathInSrcDir
      SD.createDirectoryIfMissing True (FP.takeDirectory dstFilePath)
      SD.copyFile (gitRootDirPath FP.</> filePathFromGitRoot) dstFilePath
  where
    description = "copy git-tracked contents of " ++ fromRelDir srcDirFromGitRootDir ++ " into the wasp project dir"

resolveInWorkingDir :: (HasWorkingDir ctx) => ctx -> FilePath -> FilePath
resolveInWorkingDir context path = fromAbsDir (workingDir context) FP.</> path
