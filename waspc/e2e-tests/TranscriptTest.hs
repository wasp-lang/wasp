{-# LANGUAGE DuplicateRecordFields #-}

module TranscriptTest
  ( TranscriptTest (..),
    WorkingDir (..),
    Interaction (..),
    RuntimeOutput (..),
    testTreeFromTranscriptTests,
  )
where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import FileSystem
  ( SnapshotType (..),
    SnapshotsDir,
    TestCaseDir,
    TranscriptFile,
    getSnapshotsDir,
    getTestCaseDir,
    getWaspcDirPath,
    testCaseLogFileInTestCaseDir,
    transcriptFileInTranscriptsDir,
    transcriptsDirInSnapshotsDir,
    waspCliDevToolInWaspcDir,
    waspcDataDirInWaspcDir,
  )
import ShellCommands (ShellCommand, ShellCommandBuilder, TestContext (..), WaspProjectContext (..), buildShellCommand, (~&&))
import StrongPath (Abs, Dir, File, Path', parent, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (dropTrailingPathSeparator)
import System.Process (CreateProcess (..), StdStream (..), createProcess, readProcess, shell, waitForProcess)
import Test.Tasty (DependencyType (AllFinish), TestTree, dependentTestGroup, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)
import TestLogging (formatCommandFailure, openLogForCommand)
import TranscriptTest.Normalization (NormalizationOptions (..), RuntimeOutput (..), containsRuntimeOutputOfWholeApp, normalizeOutput)
import TranscriptTest.Recording
  ( CommandToRecord (..),
    Interaction (..),
    RecordedStream (..),
    Recording (..),
    RecordingMode,
    getAvailableRecordingModes,
    recordCommand,
  )

-- | Records everything a command prints and how it exits, and compares that
-- with the golden transcript. Each test runs once per 'RecordingMode'.
data TranscriptTest = TranscriptTest
  { name :: String,
    -- | Prepares the scenario. Runs in the scenario's directory and is not
    -- recorded.
    setupCommandsBuilder :: ShellCommandBuilder TestContext [ShellCommand],
    workingDir :: WorkingDir,
    -- | The command whose output is recorded.
    command :: ShellCommand,
    interactions :: [Interaction],
    runtimeOutput :: RuntimeOutput
  }

-- | Where the recorded command runs.
data WorkingDir = ScenarioDir | WaspProjectDir

testTreeFromTranscriptTests :: [TranscriptTest] -> IO TestTree
testTreeFromTranscriptTests transcriptTests = do
  recordingModes <- getAvailableRecordingModes
  environment <- getTranscriptEnvironment
  snapshotsDir <- getSnapshotsDir
  return $
    testGroup
      "Transcript tests"
      [ -- A test's recordings use the same resources (e.g. ports), so they
      -- run one after another.
      dependentTestGroup transcriptTest.name AllFinish $
        makeGoldenTest snapshotsDir environment transcriptTest <$> recordingModes
      | transcriptTest <- transcriptTests
      ]

makeGoldenTest :: Path' Abs (Dir SnapshotsDir) -> Environment -> TranscriptTest -> RecordingMode -> TestTree
makeGoldenTest snapshotsDir environment transcriptTest recordingMode =
  goldenVsFileDiff
    (show recordingMode)
    (\goldenFilePath currentFilePath -> ["diff", "-u", goldenFilePath, currentFilePath])
    (SP.fromAbsFile $ transcriptFile Golden)
    (SP.fromAbsFile $ transcriptFile Current)
    (writeCurrentTranscript environment transcriptTest recordingMode $ transcriptFile Current)
  where
    transcriptFile snapshotType =
      snapshotsDir
        </> transcriptsDirInSnapshotsDir snapshotType
        </> transcriptFileInTranscriptsDir transcriptTest.name (show recordingMode)

writeCurrentTranscript :: Environment -> TranscriptTest -> RecordingMode -> Path' Abs (File TranscriptFile) -> IO ()
writeCurrentTranscript environment transcriptTest recordingMode currentTranscriptFile = do
  scenarioDir <- getTestCaseDir ("transcript-" ++ transcriptTest.name) (show recordingMode)
  let testContext = makeTestContext scenarioDir

  setUpScenario environment transcriptTest testContext
  recording <- recordCommand recordingMode (makeCommandToRecord testContext) transcriptTest.interactions
  transcript <- either fail return $ makeTranscript transcriptTest.runtimeOutput scenarioDir recording
  writeTranscriptFile currentTranscriptFile transcript
  where
    makeCommandToRecord testContext =
      CommandToRecord
        { command = transcriptTest.command,
          workingDir = case transcriptTest.workingDir of
            ScenarioDir -> SP.fromAbsDir testContext.testCaseDir
            WaspProjectDir -> SP.fromAbsDir testContext.waspProjectContext.waspProjectDir,
          environment
        }

makeTestContext :: Path' Abs (Dir TestCaseDir) -> TestContext
makeTestContext scenarioDir =
  TestContext
    { testCaseDir = scenarioDir,
      waspProjectContext =
        WaspProjectContext
          { waspProjectDir = scenarioDir </> [reldir|wasp-app|],
            waspProjectName = "wasp-app"
          }
    }

setUpScenario :: Environment -> TranscriptTest -> TestContext -> IO ()
setUpScenario environment transcriptTest testContext = do
  removePathForcibly scenarioDirPath
  createDirectoryIfMissing True scenarioDirPath
  unless (null setupCommands) $ runSetupCommand $ foldr1 (~&&) setupCommands
  where
    scenarioDirPath = SP.fromAbsDir testContext.testCaseDir
    setupCommands = buildShellCommand testContext transcriptTest.setupCommandsBuilder
    setupLogFile = testContext.testCaseDir </> testCaseLogFileInTestCaseDir

    runSetupCommand :: ShellCommand -> IO ()
    runSetupCommand setupCommand = do
      (logOut, logErr) <- openLogForCommand setupLogFile (transcriptTest.name ++ " / setup") setupCommand
      (_, _, _, processHandle) <-
        createProcess
          (shell setupCommand)
            { cwd = Just scenarioDirPath,
              env = Just environment,
              std_out = UseHandle logOut,
              std_err = UseHandle logErr
            }
      waitForProcess processHandle >>= \case
        ExitSuccess -> return ()
        ExitFailure code -> fail =<< formatCommandFailure code setupLogFile

type Transcript = T.Text

makeTranscript :: RuntimeOutput -> Path' Abs (Dir TestCaseDir) -> Recording -> Either String Transcript
makeTranscript runtimeOutput scenarioDir recording = do
  when (expectsRuntimeOutput && not hasRuntimeOutputOfWholeApp) $
    Left "The app didn't start: the server or the client didn't print anything."
  return $ T.concat $ (formatStream <$> transcriptStreams) ++ formattedExitCode
  where
    -- What a command prints while it stops differs between runs, and in a
    -- stream that holds none of the app's output there is no other way to
    -- tell where the cut-off transcript ends.
    transcriptStreams = case runtimeOutput of
      CutOffAtRuntimeOutput -> fromMaybe recording.streams recording.streamsBeforeCtrlC
      _keepsWholeOutput -> recording.streams

    formatStream :: RecordedStream -> T.Text
    formatStream stream =
      formatSection stream.name $ normalizeOutput normalizationOptions $ decodeOutput stream.content

    -- The app is stopped once it runs, so how the command exits is not a part
    -- of a transcript that is cut off at that point.
    formattedExitCode = case runtimeOutput of
      CutOffAtRuntimeOutput -> []
      _keepsWholeOutput -> [formatSection "exit code" $ T.pack (show recording.exitCode) <> "\n"]

    expectsRuntimeOutput = case runtimeOutput of
      NoRuntimeOutput -> False
      CutOffAtRuntimeOutput -> True
      DropRuntimeOutput -> True
    hasRuntimeOutputOfWholeApp =
      containsRuntimeOutputOfWholeApp $ T.concat $ decodeOutput . (.content) <$> recording.streams

    normalizationOptions =
      NormalizationOptions
        { runtimeOutput,
          -- Without the trailing slash, so that it also matches where the
          -- directory is printed without it.
          scenarioDir = dropTrailingPathSeparator $ SP.fromAbsDir scenarioDir
        }

    decodeOutput = T.decodeUtf8With T.lenientDecode

formatSection :: String -> T.Text -> T.Text
formatSection sectionName sectionContent =
  T.concat ["[" <> T.pack sectionName <> "]\n", sectionContent, missingNewlineMarker]
  where
    missingNewlineMarker
      | T.null sectionContent || "\n" `T.isSuffixOf` sectionContent = ""
      | otherwise = "\n<no newline at the end>\n"

writeTranscriptFile :: Path' Abs (File TranscriptFile) -> Transcript -> IO ()
writeTranscriptFile transcriptFile transcript = do
  createDirectoryIfMissing True $ SP.fromAbsDir $ parent transcriptFile
  BS.writeFile (SP.fromAbsFile transcriptFile) (T.encodeUtf8 transcript)

type Environment = [(String, String)]

-- | The environment the scenarios run in. It makes the Wasp CLI behave the
-- same on a developer's machine and on CI: as if it was run by a user, in a
-- terminal that can show colors.
getTranscriptEnvironment :: IO Environment
getTranscriptEnvironment = do
  inheritedEnvironment <- getEnvironment
  waspCliEnvironment <- getDirectWaspCliEnvironment inheritedEnvironment
  return $ overrideEnvironment (waspCliEnvironment ++ fixedEnvironment) $ dropVariables variablesToDrop inheritedEnvironment
  where
    fixedEnvironment =
      [ ("TERM", "xterm-256color"),
        ("SHELL", "/bin/sh"),
        ("WASP_TELEMETRY_DISABLE", "1")
      ]
    variablesToDrop = "NO_COLOR" : ciVariables
    -- The variables that 'Wasp.Util.checkIfOnCi' checks.
    ciVariables =
      [ "BUILD_ID",
        "BUILD_NUMBER",
        "CI",
        "CI_APP_ID",
        "CI_BUILD_ID",
        "CI_BUILD_NUMBER",
        "CI_NAME",
        "CONTINUOUS_INTEGRATION",
        "RUN_ID"
      ]

-- | The dev Wasp CLI runs through `cabal run`, and Cabal stays between us and
-- Wasp: it gets the terminal's signals too (and prints its own errors when
-- it's interrupted), and it needs the real home directory. So we run the
-- executable that Cabal built directly, the way `cabal run` would.
--
-- Any other Wasp CLI is used as it is.
getDirectWaspCliEnvironment :: Environment -> IO Environment
getDirectWaspCliEnvironment inheritedEnvironment = do
  waspcDir <- getWaspcDirPath
  let devWaspCliCmd = SP.fromAbsFile (waspcDir </> waspCliDevToolInWaspcDir)
  if lookup waspCliCmdVariable inheritedEnvironment /= Just devWaspCliCmd
    then return []
    else do
      waspCliExecutable <- T.unpack . T.strip . T.pack <$> readProcess "cabal" ["-v0", "list-bin", "wasp-cli"] ""
      return
        [ (waspCliCmdVariable, waspCliExecutable),
          ("waspc_datadir", SP.fromAbsDir (waspcDir </> waspcDataDirInWaspcDir))
        ]
  where
    waspCliCmdVariable = "WASP_CLI_CMD"

overrideEnvironment :: Environment -> Environment -> Environment
overrideEnvironment overrides environment = overrides ++ dropVariables (fst <$> overrides) environment

dropVariables :: [String] -> Environment -> Environment
dropVariables variableNames = filter ((`notElem` variableNames) . fst)
