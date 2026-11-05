{-# LANGUAGE DataKinds #-}

-- | Running commands as processes, without a shell.
module E2E.Process
  ( ProcessResult (..),
    RunningProcess,
    run,
    runExpectingFailure,
    runAllowingFailure,
    startInBackground,
    waitForOutput,
    getOutput,
    waitForExit,
    signalProcessGroupOf,
    getProcessGroupId,
  )
where

import Control.Concurrent.STM (STM, atomically, check, orElse, readTVar, registerDelay)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans.Resource (ReleaseKey, release)
import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.Command (Command (..), StdinMode (..), showCommand)
import E2E.Process.Output (Output, Stream (..), captureStream, isFullyRead, newOutput, readCombinedOutput, readStdout)
import E2E.Signals (Signal (..), signalProcessGroup)
import E2E.TestM (RunConfig (..), TestEnv (..), TestM, allocateWithCleanup, failTest, getWorkingDir, logLine, resolvePath, runTestM)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import System.Process (getPid)
import qualified System.Process.Typed as TP
import UnliftIO.Exception (catchAny)

data ProcessResult = ProcessResult
  { exitCode :: ExitCode,
    stdout :: T.Text,
    -- | Stdout and stderr interleaved in the order they arrived, like @2>&1@.
    combinedOutput :: T.Text
  }

-- | A process that runs while the test continues with other steps.
--
-- If it's still running when the test ends, it's stopped (see 'stopProcess').
data RunningProcess = RunningProcess
  { processCommand :: Command,
    process :: TP.Process () () (),
    -- | The process was started in its own group, so its PID is also its
    -- process group ID. 'Nothing' if it exited before we could read its PID.
    processGroupId :: Maybe Int,
    output :: Output
  }

-- | Runs the command and fails the test if it doesn't succeed.
run :: Command -> TestM ProcessResult
run cmd = do
  result <- runAllowingFailure cmd
  case result.exitCode of
    ExitSuccess -> return result
    ExitFailure code -> failTest $ describeFailure cmd ("exited with code " ++ show code) result.combinedOutput

-- | Runs the command and fails the test if it succeeds.
runExpectingFailure :: Command -> TestM ProcessResult
runExpectingFailure cmd = do
  result <- runAllowingFailure cmd
  case result.exitCode of
    ExitFailure _ -> return result
    ExitSuccess -> failTest $ describeFailure cmd "succeeded, but it was expected to fail" result.combinedOutput

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
    return ProcessResult {exitCode, stdout, combinedOutput}

-- | Starts the process in its own process group, so we can stop it together
-- with its children.
startInBackground :: Command -> TestM RunningProcess
startInBackground cmd = snd <$> startInBackgroundWithStopKey cmd

-- | Releasing the key stops the process. Otherwise, that happens when the test ends.
startInBackgroundWithStopKey :: Command -> TestM (ReleaseKey, RunningProcess)
startInBackgroundWithStopKey cmd = do
  executable <- resolveExecutable cmd.executable
  workingDir <- getWorkingDir
  pathOverride <- case cmd.extraPath of
    [] -> return []
    dirs -> do
      absoluteDirs <- mapM resolvePath dirs
      inheritedPath <- liftIO $ lookupEnv "PATH"
      return [("PATH", Just . intercalate [FP.searchPathSeparator] $ absoluteDirs ++ maybeToList inheritedPath)]
  environment <- liftIO $ buildEnvironment (pathOverride ++ cmd.environmentOverrides)
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
        return RunningProcess {processCommand = cmd, process, processGroupId, output}

  allocateWithCleanup start (runTestM testEnv . stopProcess)

resolveExecutable :: String -> TestM FilePath
resolveExecutable "wasp" = asks (.runConfig.waspCliPath)
resolveExecutable executable
  | any FP.isPathSeparator executable = resolvePath executable
  | otherwise = return executable

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

-- | Waits until the process prints the text (to stdout or stderr).
-- Fails the test if that doesn't happen in time or if the process exits first.
waitForOutput :: RunningProcess -> T.Text -> Int -> TestM ()
waitForOutput runningProcess expectedText seconds = do
  maybeOutcome <-
    liftIO . waitAtMost seconds $
      (outputContainsText >>= check >> return OutputFound)
        `orElse` (processExitedWithAllOutputRead >>= check >> return ProcessExited)
  output <- getOutput runningProcess
  case maybeOutcome of
    Just OutputFound -> logLine $ "\n==> Found expected output: " ++ show expectedText
    Just ProcessExited -> failTest $ describeFailure runningProcess.processCommand ("exited before printing " ++ show expectedText) output
    Nothing -> failTest $ describeFailure runningProcess.processCommand ("didn't print " ++ show expectedText ++ " within " ++ show seconds ++ "s") output
  where
    outputContainsText :: STM Bool
    outputContainsText = T.isInfixOf expectedText <$> readCombinedOutput runningProcess.output

    processExitedWithAllOutputRead :: STM Bool
    processExitedWithAllOutputRead = (&&) . isJust <$> TP.getExitCodeSTM runningProcess.process <*> isFullyRead runningProcess.output

data WaitForOutputOutcome = OutputFound | ProcessExited

-- | Stdout and stderr the process printed so far, interleaved.
getOutput :: RunningProcess -> TestM T.Text
getOutput runningProcess = liftIO . atomically $ readCombinedOutput runningProcess.output

-- | Waits for the process to exit, but at most for the given number of seconds.
-- If it doesn't exit in time, the test fails (and the process is stopped).
waitForExit :: RunningProcess -> Int -> TestM ExitCode
waitForExit runningProcess seconds = do
  maybeExitCode <- liftIO . waitAtMost seconds $ TP.waitExitCodeSTM runningProcess.process
  case maybeExitCode of
    Nothing -> do
      output <- getOutput runningProcess
      failTest $ describeFailure runningProcess.processCommand ("didn't exit within " ++ show seconds ++ "s") output
    Just exitCode -> do
      -- A process can exit before we've read all of its output. We give the
      -- readers a moment to finish. We don't wait longer since children that
      -- outlive the process (e.g. daemons) can keep the pipes open.
      liftIO . void . waitAtMost 5 $ isFullyRead runningProcess.output >>= check
      logLine $ "<== " ++ showExitCode exitCode
      return exitCode

-- | Sends the signal to the process group the process runs in, like
-- a terminal does for Ctrl+C (SIGINT) or when it's closed (SIGHUP).
signalProcessGroupOf :: Signal -> RunningProcess -> TestM ()
signalProcessGroupOf sig runningProcess = do
  processGroupId <- getProcessGroupId runningProcess
  logLine $ "\n==> Sending " ++ show sig ++ " to the process group of: " ++ showCommand runningProcess.processCommand
  liftIO $ signalProcessGroup sig processGroupId

-- | The ID of the process group the process runs in.
getProcessGroupId :: RunningProcess -> TestM Int
getProcessGroupId runningProcess =
  maybe
    (failTest $ "Command `" ++ showCommand runningProcess.processCommand ++ "` exited before its process group ID could be read.")
    return
    runningProcess.processGroupId

-- | Interrupts the process group if the process is still running, and kills
-- the group if the process doesn't exit soon after. Finally kills whatever is
-- left of the group, since children can outlive the process.
stopProcess :: RunningProcess -> TestM ()
stopProcess runningProcess = do
  isRunning <- liftIO $ isNothing <$> TP.getExitCode runningProcess.process
  when isRunning $ do
    logLine $ "\n==> Stopping: " ++ showCommand runningProcess.processCommand
    signalGroup SIGINT
    maybeExitCode <- liftIO . waitAtMost 10 $ TP.waitExitCodeSTM runningProcess.process
    logLine $ maybe "<== didn't stop within 10s, killing it" (("<== " ++) . showExitCode) maybeExitCode
  signalGroup SIGKILL
  -- Only once the process is killed: this sends it SIGTERM, which it can
  -- ignore, and then waits for it to exit without a time limit.
  liftIO $ TP.stopProcess runningProcess.process
  where
    signalGroup sig = liftIO . forM_ runningProcess.processGroupId $ \processGroupId ->
      signalProcessGroup sig processGroupId `catchAny` const (return ())

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

showExitCode :: ExitCode -> String
showExitCode ExitSuccess = "exit code 0"
showExitCode (ExitFailure code)
  -- That's how 'waitForProcess' reports processes stopped by a signal.
  | code < 0 = "stopped by signal " ++ show (negate code)
  | otherwise = "exit code " ++ show code
