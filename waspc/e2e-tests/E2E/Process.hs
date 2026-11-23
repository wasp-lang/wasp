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
    waitForExit,
    assertExitsSuccessfully,
    assertExitsWithFailure,
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
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.Command (Command (..), Executable (..), StdinMode (..), showCommand)
import E2E.Process.Output (Output, Stream (..), captureStream, isFullyRead, newOutput, readCombinedOutput, readStdout)
import E2E.RunConfig (RunConfig (..))
import E2E.Signals (Signal (..), processGroupExists, signalProcess, signalProcessGroup)
import E2E.TestM (TestEnv (..), TestM, allocateWithCleanup, failTest, getWorkingDir, logLine, runTestM)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
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
run cmd = do
  result <- runAllowingFailure cmd
  mapM_ (failWithResult result) $ unexpectedExitProblem ExpectSuccess result.exitCode
  return result

-- | Runs the command and fails the test if it doesn't exit with an error,
-- i.e. if it succeeds or is stopped by a signal (e.g. when it crashes).
runExpectingFailure :: Command -> TestM ProcessResult
runExpectingFailure cmd = do
  result <- runAllowingFailure cmd
  mapM_ (failWithResult result) $ unexpectedExitProblem ExpectFailure result.exitCode
  return result

runAllowingFailure :: Command -> TestM ProcessResult
runAllowingFailure cmd = do
  (stopKey, runningProcess) <- startInBackgroundWithStopKey cmd
  exitCode <- waitForExit runningProcess cmd.timeoutSeconds
  -- Stops what the command left running right away, instead of when the test
  -- ends. By then, its process group ID could belong to another group.
  release stopKey
  liftIO . atomically $ do
    stdout <- readStdout runningProcess.output
    combinedOutput <- readCombinedOutput runningProcess.output
    return ProcessResult {processCommand = cmd, exitCode, stdout, combinedOutput}

-- | Starts the process in its own process group, so we can stop it together
-- with its children.
startInBackground :: Command -> TestM RunningProcess
startInBackground cmd = snd <$> startInBackgroundWithStopKey cmd

-- | Releasing the key stops the process. Otherwise, that happens when the test ends.
startInBackgroundWithStopKey :: Command -> TestM (ReleaseKey, RunningProcess)
startInBackgroundWithStopKey cmd = do
  executable <- case cmd.executable of
    WaspCli -> asks (.runConfig.waspCliPath)
    Program program -> return program
  workingDir <- getWorkingDir
  environment <- liftIO $ buildEnvironment cmd.environmentOverrides
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

stdinSpec :: StdinMode -> TP.StreamSpec 'TP.STInput ()
stdinSpec = \case
  -- Written from another thread, since a process that is blocked writing to
  -- a full output pipe doesn't read its stdin.
  StdinText text -> TP.byteStringInput (BSL.fromStrict $ TE.encodeUtf8 text)
  OpenEmptyStdin -> void TP.createPipe

buildEnvironment :: [(String, Maybe String)] -> IO (Maybe [(String, String)])
buildEnvironment [] = return Nothing -- Inherit the test runner's environment.
buildEnvironment overrides = Just . applyOverrides <$> getEnvironment
  where
    applyOverrides environment = foldl applyOverride environment overrides
    applyOverride environment (key, maybeValue) =
      filter ((/= key) . fst) environment ++ maybe [] (\value -> [(key, value)]) maybeValue

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
  when exited $ failWithOutput runningProcess ("exited before " ++ expectation)

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

-- | Waits for the process to exit, but at most for the given number of seconds.
-- If it doesn't exit in time, the test fails (and the process is stopped).
waitForExit :: RunningProcess -> Int -> TestM ExitCode
waitForExit runningProcess seconds = do
  maybeExitCode <- liftIO . waitAtMost seconds $ TP.waitExitCodeSTM runningProcess.process
  case maybeExitCode of
    Nothing -> failWithOutput runningProcess ("didn't exit within " ++ show seconds ++ "s")
    Just exitCode -> do
      -- A process can exit before we've read all of its output. We give the
      -- readers a moment to finish. We don't wait longer since children that
      -- outlive the process (e.g. daemons) can keep the pipes open.
      liftIO . void . waitAtMost outputDrainSeconds $ isFullyRead runningProcess.output >>= check
      logLine $ "\n<== " ++ describeExitCode exitCode
      -- The process is reaped, so if its group is empty now, the OS can
      -- reuse the group ID, and we must remember not to signal it. The
      -- process did nothing wrong if the check fails, so we don't fail the
      -- test: the group just isn't marked as gone, and later steps check again.
      liftIO (forM_ runningProcess.processGroupId $ checkProcessGroupExists runningProcess)
        `catchAny` const (return ())
      return exitCode

-- | Waits for the process to exit, but at most for the given number of
-- seconds, and fails the test with its output if it doesn't exit in time or
-- doesn't succeed.
assertExitsSuccessfully :: RunningProcess -> Int -> TestM ()
assertExitsSuccessfully runningProcess seconds =
  waitForExit runningProcess seconds
    >>= mapM_ (failWithOutput runningProcess) . unexpectedExitProblem ExpectSuccess

-- | Like 'assertExitsSuccessfully', but the process must exit with an error,
-- and not be stopped by a signal (e.g. when it crashes).
assertExitsWithFailure :: RunningProcess -> Int -> TestM ()
assertExitsWithFailure runningProcess seconds =
  waitForExit runningProcess seconds
    >>= mapM_ (failWithOutput runningProcess) . unexpectedExitProblem ExpectFailure

data ExpectedExit = ExpectSuccess | ExpectFailure

-- | What's wrong with the exit code, if anything, for a failure message.
unexpectedExitProblem :: ExpectedExit -> ExitCode -> Maybe String
unexpectedExitProblem ExpectSuccess ExitSuccess = Nothing
unexpectedExitProblem ExpectSuccess exitCode = Just $ describeExitCode exitCode
unexpectedExitProblem ExpectFailure ExitSuccess = Just "succeeded, but it was expected to fail"
unexpectedExitProblem ExpectFailure exitCode@(ExitFailure code)
  | isStoppedBySignal code = Just $ describeExitCode exitCode ++ ", but it was expected to exit with an error"
  | otherwise = Nothing

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

    -- How long a process has to stop on its own when the test ends.
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
      ++ map (("  " ++) . T.unpack) (lastLines 30 output)
  where
    lastLines count = reverse . take count . reverse . T.lines

-- | E.g. "exited with code 1", for logs and failure messages.
describeExitCode :: ExitCode -> String
describeExitCode ExitSuccess = "exited with code 0"
describeExitCode (ExitFailure code)
  | isStoppedBySignal code = "was stopped by signal " ++ show (negate code)
  | otherwise = "exited with code " ++ show code

-- | That's how 'waitForProcess' reports processes stopped by a signal.
isStoppedBySignal :: Int -> Bool
isStoppedBySignal code = code < 0
