import Control.Concurrent (getNumCapabilities)
import Control.Monad (unless, when)
import Data.List (isInfixOf)
import E2e.Paths (getWaspcDir, gitRootDirFromWaspcDir)
import E2e.Resource (Resource (..))
import E2e.Runner (RunOptions (..), runTests)
import E2e.Snapshot (SnapshotMode (..))
import E2e.Test (E2eTest (..), TestKind (..), fullTestName)
import E2e.TestM (RunConfig (..))
import qualified Options.Applicative as Opt
import StrongPath ((</>))
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Tests (allTests)
import Text.Read (readMaybe)

data CliOptions = CliOptions
  { jobs :: Maybe Int,
    accept :: Bool,
    selectedKind :: Maybe TestKind,
    matches :: [String],
    list :: Bool
  }

main :: IO ()
main =
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows."
    else do
      -- Progress must show up right away, even when stdout is not a terminal (e.g. in CI).
      hSetBuffering stdout LineBuffering
      cliOptions <- Opt.execParser cliOptionsParser
      let selectedTests =
            filter (isSelected cliOptions.matches)
              . filter (\e2eTest -> maybe True (isOfKind e2eTest) cliOptions.selectedKind)
              $ allTests
      when (null selectedTests) $ do
        putStrLn "No tests match the given options."
        exitFailure
      if cliOptions.list
        then mapM_ (putStrLn . describeTest) selectedTests
        else do
          runConfig <- makeRunConfig
          maxParallelJobs <- maybe getDefaultMaxParallelJobs return cliOptions.jobs
          let snapshotMode = if cliOptions.accept then AcceptCurrent else CompareToGolden
          allPassed <- runTests runConfig RunOptions {maxParallelJobs, snapshotMode} selectedTests
          unless allPassed exitFailure

isOfKind :: E2eTest -> TestKind -> Bool
isOfKind e2eTest testKind = case (e2eTest.kind, testKind) of
  (PlainTest, PlainTest) -> True
  (SnapshotTest, SnapshotTest) -> True
  _ -> False

isSelected :: [String] -> E2eTest -> Bool
isSelected [] _ = True
isSelected patterns e2eTest = any (`isInfixOf` fullTestName e2eTest) patterns

describeTest :: E2eTest -> String
describeTest e2eTest =
  fullTestName e2eTest
    ++ concat [" [uses " ++ resource.name ++ "]" | resource <- e2eTest.resources]
    ++ (if e2eTest.requiresDocker then " [requires docker]" else "")

cliOptionsParser :: Opt.ParserInfo CliOptions
cliOptionsParser =
  Opt.info (parser Opt.<**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Runs the waspc e2e tests.")
  where
    parser =
      CliOptions
        <$> Opt.optional
          ( Opt.option
              Opt.auto
              (Opt.long "jobs" <> Opt.short 'j' <> Opt.metavar "N" <> Opt.help "Max number of tests running at once. Defaults to $WASP_E2E_TEST_MAX_JOBS or half of the CPU cores.")
          )
        <*> Opt.switch (Opt.long "accept" <> Opt.help "Replace golden snapshots with the current ones.")
        <*> Opt.optional
          ( Opt.option
              (Opt.eitherReader parseTestKind)
              (Opt.long "kind" <> Opt.metavar "plain|snapshot" <> Opt.help "Only run tests of this kind. `--kind snapshot --accept` regenerates the golden snapshots without running the other tests.")
          )
        <*> Opt.many (Opt.strOption (Opt.long "match" <> Opt.short 'm' <> Opt.metavar "TEXT" <> Opt.help "Only run tests whose name contains TEXT. Repeatable."))
        <*> Opt.switch (Opt.long "list" <> Opt.help "List the tests instead of running them.")

parseTestKind :: String -> Either String TestKind
parseTestKind = \case
  "plain" -> Right PlainTest
  "snapshot" -> Right SnapshotTest
  other -> Left $ "Unknown test kind " ++ show other ++ ", expected plain or snapshot."

makeRunConfig :: IO RunConfig
makeRunConfig = do
  waspcDir <- getWaspcDir
  waspCliPath <- findWaspCli
  warmUpWaspCli waspCliPath
  isDockerEnabled <- (`elem` [Nothing, Just ""]) <$> lookupEnv "WASP_E2E_TESTS_SKIP_DOCKER"
  return RunConfig {waspCliPath, gitRootDir = waspcDir </> gitRootDirFromWaspcDir, isDockerEnabled}

-- | Prefers @WASP_CLI_CMD@ (e.g. @tools/wasp-cli-dev@ from @./run@), and falls
-- back to the @wasp-cli@ that Cabal builds and puts on the PATH for this test suite.
findWaspCli :: IO FilePath
findWaspCli =
  lookupEnv "WASP_CLI_CMD" >>= \case
    Just waspCliCmd | not (null waspCliCmd) -> return waspCliCmd
    _ -> findExecutable "wasp-cli" >>= maybe (fail "Couldn't find the wasp-cli executable. Set WASP_CLI_CMD.") return

-- | When the CLI runs through `cabal run`, its first call builds it. Several
-- concurrent first calls race on the same build dir, so we make one call upfront.
warmUpWaspCli :: FilePath -> IO ()
warmUpWaspCli waspCliPath = do
  (exitCode, _, err) <- readProcessWithExitCode waspCliPath ["version"] ""
  when (exitCode /= ExitSuccess) $ fail $ "Running `" ++ waspCliPath ++ " version` failed:\n" ++ err

-- | Each test runs Node tooling (npm, tsc, vite) that already uses several cores.
getDefaultMaxParallelJobs :: IO Int
getDefaultMaxParallelJobs =
  lookupEnv "WASP_E2E_TEST_MAX_JOBS" >>= \case
    Just jobsText | Just jobs <- readMaybe jobsText, jobs >= 1 -> return jobs
    _ -> max 1 . (`div` 2) <$> getNumCapabilities
