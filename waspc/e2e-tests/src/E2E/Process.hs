{-# LANGUAGE DataKinds #-}

-- | Running commands as processes, without a shell.
--
-- We use @typed-process@ over @process@ for its STM exit codes, which let
-- 'waitForOutput' race a process's output against its exit, and @resourcet@
-- (through 'allocateWithCleanup') to register a process's cleanup in the same
-- masked step that starts it, so a cancelled test can't leave it running.
module E2E.Process
  ( ProcessResult (..),
    RunningProcess,
    run,
    runExpectingFailure,
    runAllowingFailure,
    startInBackground,
    waitForOutput,
    failIfExited,
    failWithOutput,
    failWithResult,
    getOutput,
    stdoutLines,
    firstStdoutLine,
    decodeStdoutJson,
    waitForExit,
    waitUntilExited,
    stopTimeoutSeconds,
    readResult,
    ExpectedExit (..),
    failOnUnexpectedExit,
    signalPid,
    signalProcessGroupOf,
    processGroupOfExists,
  )
where

import Control.Concurrent.STM (STM, atomically, check, orElse, readTVar, registerDelay)
import Control.Exception (IOException)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans.Resource (ReleaseKey, release)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.Command (Command (..), Executable (..), StdinMode (..), showCommand)
import E2E.Process.Output (Output, Stream (..), captureStream, isFullyRead, newOutput, readCombinedOutput, readStdout)
import E2E.Signals (Signal (..), processGroupExists, signalProcess, signalProcessGroup)
import E2E.TestM (TestEnv (..), TestM, allocateWithCleanup, failTest, getWorkingDir, logLine, resolvePath, runTestM)
import E2E.TestSuiteEnv (TestSuiteEnv (..))
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import System.Process (getPid)
import qualified System.Process.Typed as TP
import UnliftIO.Exception (catch, catchAny, displayException)

data ProcessResult = ProcessResult
  { processCommand :: Command,
    exitCode :: ExitCode,
    stdout :: T.Text,
    -- | Stdout and stderr interleaved in the order they arrived, like @2>&1@.
    combinedOutput :: T.Text
  }

-- | A process that runs while the test continues with other steps.
--
-- If it's still running when the test ends, it's stopped (see 'stopProcess').
data RunningProcess = RunningProcess
  { runningCommand :: Command,
    process :: TP.Process () () (),
    -- | The process was started in its own group, so its PID is also its
    -- process group ID. 'Nothing' if it exited before we could read its PID.
    processGroupId :: Maybe Int,
    -- | Set once we saw the process exit and its group empty. From then on,
    -- the OS can give the group ID to a new process group, so we must not
    -- signal it anymore. This doesn't cover a group that empties while no
    -- step checks it (e.g. the children of an exited process die later): its
    -- ID can be reused before we check again. Only pidfds could close that gap.
    processGroupGone :: IORef Bool,
    output :: Output
  }

-- | Runs the command and fails the test if it doesn't succeed.
run :: Command -> TestM ProcessResult
run = runExpecting ExpectSuccess

-- | Runs the command and fails the test if it doesn't exit with an error,
-- i.e. if it succeeds or is stopped by a signal (e.g. when it crashes).
runExpectingFailure :: Command -> TestM ProcessResult
runExpectingFailure = runExpecting ExpectFailure

runExpecting :: ExpectedExit -> Command -> TestM ProcessResult
runExpecting expectedExit cmd = do
  result <- runAllowingFailure cmd
  failOnUnexpectedExit expectedExit result
  return result

runAllowingFailure :: Command -> TestM ProcessResult
runAllowingFailure cmd = do
  (stopKey, runningProcess) <- startInBackgroundWithStopKey cmd
  exitCode <- waitForExit runningProcess cmd.timeoutSeconds
  -- Stops what the command left running right away, instead of when the test
  -- ends. By then, its process group ID could belong to another group.
  release stopKey
  readResult runningProcess exitCode

-- | The result of a process that exited with the exit code.
readResult :: RunningProcess -> ExitCode -> TestM ProcessResult
readResult runningProcess exitCode =
  liftIO . atomically $ do
    stdout <- readStdout runningProcess.output
    combinedOutput <- readCombinedOutput runningProcess.output
    return ProcessResult {processCommand = runningProcess.runningCommand, exitCode, stdout, combinedOutput}

-- | Starts the process in its own process group, so we can stop it together
-- with its children.
startInBackground :: Command -> TestM RunningProcess
startInBackground cmd = snd <$> startInBackgroundWithStopKey cmd

-- | Releasing the key stops the process. Otherwise, that happens when the test ends.
startInBackgroundWithStopKey :: Command -> TestM (ReleaseKey, RunningProcess)
startInBackgroundWithStopKey cmd = do
  executable <- resolveExecutable cmd.executable
  workingDir <- getWorkingDir
  environment <- buildEnvironment cmd
  testEnv <- ask
  output <- liftIO $ newOutput testEnv.testLog
  logLine $ "\n==> " ++ showCommand cmd ++ "\n    in " ++ workingDir

  let processConfig =
        TP.setCreateGroup True
          . TP.setWorkingDir workingDir
          . maybe id TP.setEnv environment
          . TP.setStdin (stdinSpec cmd.stdinMode)
          . TP.setStdout (captureStream output Stdout)
          . TP.setStderr (captureStream output Stderr)
          $ TP.proc executable cmd.arguments
      start = do
        process <- TP.startProcess processConfig
        processGroupId <- fmap fromIntegral <$> getPid (TP.unsafeProcessHandle process)
        processGroupGone <- newIORef False
        return RunningProcess {runningCommand = cmd, process, processGroupId, processGroupGone, output}

  allocateWithCleanup start (runTestM testEnv . stopProcess)

-- | We make relative paths absolute ourselves, since with a working directory
-- set, it depends on the platform whether @process@ resolves them against it
-- or against ours.
resolveExecutable :: Executable -> TestM FilePath
resolveExecutable WaspCli = asks (.testSuiteEnv.waspCliPath)
resolveExecutable (Program program)
  | any FP.isPathSeparator program = resolvePath program
  | otherwise = return program

stdinSpec :: StdinMode -> TP.StreamSpec 'TP.STInput ()
stdinSpec = \case
  -- Written from another thread, since a process that is blocked writing to
  -- a full output pipe doesn't read its stdin.
  StdinText text -> TP.byteStringInput (BSL.fromStrict $ TE.encodeUtf8 text)
  OpenEmptyStdin -> void TP.createPipe

-- | 'Nothing' inherits the test runner's environment.
--
-- The command's extra path dirs go before the @PATH@ that its environment
-- overrides leave, so a @PATH@ set with 'E2E.Command.withEnv' doesn't drop
-- them.
buildEnvironment :: Command -> TestM (Maybe [(String, String)])
buildEnvironment cmd = case (cmd.extraPath, cmd.environmentOverrides) of
  ([], []) -> return Nothing
  (extraPathDirs, overrides) -> do
    absoluteExtraPathDirs <- mapM resolvePath extraPathDirs
    environment <- liftIO getEnvironment
    return . Just . prependToPath absoluteExtraPathDirs $ foldl applyOverride environment overrides
  where
    applyOverride environment (key, maybeValue) =
      withoutKey key environment ++ maybe [] (\value -> [(key, value)]) maybeValue

    prependToPath [] environment = environment
    prependToPath dirs environment =
      withoutKey "PATH" environment
        ++ [("PATH", intercalate [FP.searchPathSeparator] $ dirs ++ maybeToList (lookup "PATH" environment))]

    withoutKey key = filter ((/= key) . fst)

-- | Waits until the process prints the text (to stdout or stderr), or returns
-- right away if it already did, also before this step.
-- Fails the test if that doesn't happen in time or if the process exits first.
waitForOutput :: RunningProcess -> T.Text -> Int -> TestM ()
waitForOutput runningProcess expectedText seconds = do
  maybeOutcome <-
    liftIO . waitAtMost seconds $
      (outputContainsText >>= check >> return OutputFound)
        `orElse` (processExitedWithAllOutputRead >>= check >> return ProcessExited)
  case maybeOutcome of
    Just OutputFound -> logLine $ "\n==> Found expected output: " ++ show expectedText
    Just ProcessExited -> failWithOutput runningProcess ("exited before printing " ++ show expectedText)
    Nothing -> failWithOutput runningProcess ("didn't print " ++ show expectedText ++ " within " ++ show seconds ++ "s")
  where
    outputContainsText :: STM Bool
    outputContainsText = T.isInfixOf expectedText <$> readCombinedOutput runningProcess.output

    processExitedWithAllOutputRead :: STM Bool
    processExitedWithAllOutputRead = (&&) . isJust <$> TP.getExitCodeSTM runningProcess.process <*> isFullyRead runningProcess.output

data WaitForOutputOutcome = OutputFound | ProcessExited

-- | Fails the test if the process has exited, e.g. while a test polls for
-- something the process should do, so that it fails with the process's output
-- right away instead of when the poll times out.
failIfExited :: RunningProcess -> String -> TestM ()
failIfExited runningProcess expectation = do
  exited <- liftIO $ isJust <$> TP.getExitCode runningProcess.process
  when exited $ do
    -- So that the failure shows the last lines it printed, e.g. why it crashed.
    waitForOutputDrain runningProcess
    failWithOutput runningProcess ("exited before " ++ expectation)

-- | Fails the test with the problem and the last lines the process printed so
-- far, e.g. "didn't open its port".
failWithOutput :: RunningProcess -> String -> TestM a
failWithOutput runningProcess problem =
  getOutput runningProcess >>= failTest . describeFailure runningProcess.runningCommand problem

-- | Like 'failWithOutput', for a process that already exited.
failWithResult :: ProcessResult -> String -> TestM a
failWithResult result problem = failTest $ describeFailure result.processCommand problem result.combinedOutput

-- | Stdout and stderr the process printed so far, interleaved.
getOutput :: RunningProcess -> TestM T.Text
getOutput runningProcess = liftIO . atomically $ readCombinedOutput runningProcess.output

-- | The non-empty lines of stdout, without surrounding whitespace.
stdoutLines :: ProcessResult -> [String]
stdoutLines = filter (not . null) . map (T.unpack . T.strip) . T.lines . (.stdout)

-- | The first of 'stdoutLines'. Fails the test if there is none.
firstStdoutLine :: ProcessResult -> TestM String
firstStdoutLine result = case stdoutLines result of
  firstLine : _ -> return firstLine
  [] -> failWithResult result "printed nothing to stdout"

-- | Stdout decoded as JSON, e.g. into an 'Aeson.Object' to check its fields.
-- Fails the test if it doesn't decode.
decodeStdoutJson :: (Aeson.FromJSON a) => ProcessResult -> TestM a
decodeStdoutJson result =
  either (failWithResult result . ("printed JSON to stdout that doesn't decode: " ++)) return $
    Aeson.eitherDecodeStrict (TE.encodeUtf8 result.stdout)

-- | Waits for the process to exit, but at most for the given number of seconds.
-- If it doesn't exit in time, the test fails (and the process is stopped).
waitForExit :: RunningProcess -> Int -> TestM ExitCode
waitForExit runningProcess seconds = do
  maybeExitCode <- liftIO . waitAtMost seconds $ TP.waitExitCodeSTM runningProcess.process
  case maybeExitCode of
    Nothing -> failWithOutput runningProcess ("didn't exit within " ++ show seconds ++ "s")
    Just exitCode -> do
      -- The process is reaped, so if its group is empty now, the OS can
      -- reuse the group ID, and we must remember not to signal it. We check
      -- before waiting for the output, so that the window for that is short.
      -- The process did nothing wrong if the check fails, so we don't fail the
      -- test: the group just isn't marked as gone, and later steps check again.
      liftIO (forM_ runningProcess.processGroupId $ checkProcessGroupExists runningProcess)
        `catchAny` const (return ())
      waitForOutputDrain runningProcess
      logLine $ "\n<== " ++ describeExitCode exitCode
      return exitCode

-- | Waits for a process we told to stop (e.g. with a signal) to exit, within
-- 'stopTimeoutSeconds'.
waitUntilExited :: RunningProcess -> TestM ()
waitUntilExited runningProcess = void $ waitForExit runningProcess stopTimeoutSeconds

-- | How long a process has to exit after we told it to stop, or after
-- something it depends on crashed.
stopTimeoutSeconds :: Int
stopTimeoutSeconds = 60

-- | 'ExpectFailure' means exiting with an error, and not being stopped by
-- a signal (e.g. when it crashes).
data ExpectedExit = ExpectSuccess | ExpectFailure

-- | Fails the test with the process's output if it didn't exit as expected.
failOnUnexpectedExit :: ExpectedExit -> ProcessResult -> TestM ()
failOnUnexpectedExit expectedExit result =
  mapM_ (failWithResult result) $ unexpectedExitProblem expectedExit result.exitCode

-- | What's wrong with the exit code, if anything, for a failure message.
unexpectedExitProblem :: ExpectedExit -> ExitCode -> Maybe String
unexpectedExitProblem ExpectSuccess ExitSuccess = Nothing
unexpectedExitProblem ExpectSuccess exitCode = Just $ describeExitCode exitCode
unexpectedExitProblem ExpectFailure ExitSuccess = Just "succeeded, but it was expected to fail"
unexpectedExitProblem ExpectFailure exitCode@(ExitFailure code)
  | isStoppedBySignal code = Just $ describeExitCode exitCode ++ ", but it was expected to exit with an error"
  | otherwise = Nothing

-- | A process can exit before we've read all of its output. We give the
-- readers a moment to finish, but not longer, since children that outlive the
-- process (e.g. daemons) can keep the pipes open.
waitForOutputDrain :: RunningProcess -> TestM ()
waitForOutputDrain runningProcess =
  liftIO . void . waitAtMost outputDrainSeconds $ isFullyRead runningProcess.output >>= check

-- | How long we wait for a process's output after it exits.
outputDrainSeconds :: Int
outputDrainSeconds = 5

-- | Sends the signal to the process with the PID, e.g. one read from the
-- project lock file.
signalPid :: Signal -> Int -> TestM ()
signalPid sig pid = do
  logLine $ "\n==> Sending " ++ show sig ++ " to PID " ++ show pid
  liftIO (signalProcess sig pid) `catch` \(err :: IOException) ->
    failTest $ "Couldn't send " ++ show sig ++ " to PID " ++ show pid ++ ": " ++ displayException err

-- | Sends the signal to the process group the process runs in, like
-- a terminal does for Ctrl+C (SIGINT) or when it's closed (SIGHUP).
-- Fails the test if the group has no processes left.
signalProcessGroupOf :: Signal -> RunningProcess -> TestM ()
signalProcessGroupOf sig runningProcess = do
  processGroupId <- getProcessGroupId runningProcess
  isGroupGone <- liftIO $ readIORef runningProcess.processGroupGone
  when isGroupGone $
    failWithOutput runningProcess ("has no processes left in its process group to send " ++ show sig ++ " to")
  logLine $ "\n==> Sending " ++ show sig ++ " to the process group of: " ++ showCommand runningProcess.runningCommand
  liftIO (signalProcessGroup sig processGroupId) `catch` \(err :: IOException) ->
    failWithOutput runningProcess ("couldn't be sent " ++ show sig ++ " to its process group: " ++ displayException err)

-- | Whether any process is left in the process group the process ran in, e.g.
-- children that outlive it.
processGroupOfExists :: RunningProcess -> TestM Bool
processGroupOfExists runningProcess = do
  processGroupId <- getProcessGroupId runningProcess
  liftIO (checkProcessGroupExists runningProcess processGroupId) `catch` \(err :: IOException) ->
    failWithOutput runningProcess ("couldn't be checked for processes left in its process group: " ++ displayException err)

-- | Once the group is gone, we remember it and don't check again, since the OS
-- can give its ID to a new group.
checkProcessGroupExists :: RunningProcess -> Int -> IO Bool
checkProcessGroupExists runningProcess processGroupId = do
  isGroupGone <- readIORef runningProcess.processGroupGone
  if isGroupGone
    then return False
    else do
      exists <- processGroupExists processGroupId
      unless exists $ writeIORef runningProcess.processGroupGone True
      return exists

getProcessGroupId :: RunningProcess -> TestM Int
getProcessGroupId runningProcess =
  maybe
    (failWithOutput runningProcess "exited before its process group ID could be read")
    return
    runningProcess.processGroupId

-- | Interrupts the process group if the process is still running, and kills
-- the group if the process doesn't exit soon after. Finally kills whatever is
-- left of the group, since children can outlive the process.
stopProcess :: RunningProcess -> TestM ()
stopProcess runningProcess = do
  isRunning <- liftIO $ isNothing <$> TP.getExitCode runningProcess.process
  when isRunning $ do
    logLine $ "\n==> Stopping: " ++ showCommand runningProcess.runningCommand
    signalGroup SIGINT
    maybeExitCode <- liftIO . waitAtMost stopGracePeriodSeconds $ TP.waitExitCodeSTM runningProcess.process
    logLine $
      maybe
        ("\n<== didn't stop within " ++ show stopGracePeriodSeconds ++ "s, killing it")
        (("\n<== " ++) . describeExitCode)
        maybeExitCode
  signalGroup SIGKILL
  -- Only once the process is killed: this sends it SIGTERM, which it can
  -- ignore, and then waits for it to exit without a time limit.
  liftIO $ TP.stopProcess runningProcess.process
  -- The test's log is closed once the test ends, so we let the readers write
  -- the process's last output to it first.
  waitForOutputDrain runningProcess
  where
    -- Only while the group exists, since its ID could belong to a new group
    -- once it's gone (but see 'processGroupGone' for when we can't tell).
    -- Errors are only logged, so that we still stop the process: the signal
    -- fails e.g. when the group's last process exits right after the check,
    -- which is what we want anyway.
    signalGroup sig = forM_ runningProcess.processGroupId $ \processGroupId -> do
      let signalIfGroupExists = do
            exists <- checkProcessGroupExists runningProcess processGroupId
            when exists $ signalProcessGroup sig processGroupId
      liftIO signalIfGroupExists `catchAny` \err ->
        logLine $ "\n<== Couldn't send " ++ show sig ++ " to its process group: " ++ displayException err

-- | How long a process has to stop on its own when the test ends.
stopGracePeriodSeconds :: Int
stopGracePeriodSeconds = 10

-- | Runs the transaction once it succeeds, or returns 'Nothing' if that doesn't
-- happen within the given number of seconds.
waitAtMost :: Int -> STM a -> IO (Maybe a)
waitAtMost seconds transaction = do
  timedOut <- registerDelay (seconds * 1000000)
  atomically $ (Just <$> transaction) `orElse` (readTVar timedOut >>= check >> return Nothing)

describeFailure :: Command -> String -> T.Text -> String
describeFailure cmd problem output =
  unlines $
    ["Command `" ++ showCommand cmd ++ "` " ++ problem ++ ".", "Last lines of its output:"]
      ++ map (("  " ++) . T.unpack) (lastLines failureOutputLineCount output)
  where
    lastLines count = reverse . take count . reverse . T.lines

-- | How many lines of a command's output a failure message shows. The rest is
-- in the test's log.
failureOutputLineCount :: Int
failureOutputLineCount = 30

-- | E.g. "exited with code 1", for logs and failure messages.
describeExitCode :: ExitCode -> String
describeExitCode ExitSuccess = "exited with code 0"
describeExitCode (ExitFailure code)
  | isStoppedBySignal code = "was stopped by signal " ++ show (negate code)
  | otherwise = "exited with code " ++ show code

-- | That's how 'waitForProcess' reports processes stopped by a signal.
isStoppedBySignal :: Int -> Bool
isStoppedBySignal code = code < 0
