{-# LANGUAGE DataKinds #-}

-- | Running commands as processes, without a shell.
module E2E.Process
  ( ProcessResult (..),
    BackgroundProcess,
    run,
    runExpectingFailure,
    runAllowingFailure,
    startInBackground,
    waitForOutput,
    getBackgroundOutput,
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
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.Command (Command (..), StdinMode (..), showCommand)
import E2E.Process.Output (Output, Stream (..), captureStream, isFullyRead, newOutput, readCombinedOutput, readStdout)
import E2E.Signals (Signal (..), signalProcessGroup)
import E2E.TestM (RunConfig (..), TestEnv (..), TestM, allocateWithCleanup, failTest, getWorkingDir, logLine, runTestM)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
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
data BackgroundProcess = BackgroundProcess
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
  (stopKey, backgroundProcess) <- startInBackgroundWithStopKey cmd
  exitCode <- waitForExit backgroundProcess cmd.timeoutSeconds
  -- Stops what the command left running right away, instead of when the test
  -- ends. By then, its process group ID could belong to another group.
  release stopKey
  liftIO . atomically $ do
    stdout <- readStdout backgroundProcess.output
    combinedOutput <- readCombinedOutput backgroundProcess.output
    return ProcessResult {exitCode, stdout, combinedOutput}

-- | Starts the process in its own process group, so we can stop it together
-- with its children.
startInBackground :: Command -> TestM BackgroundProcess
startInBackground cmd = snd <$> startInBackgroundWithStopKey cmd

-- | Releasing the key stops the process. Otherwise, that happens when the test ends.
startInBackgroundWithStopKey :: Command -> TestM (ReleaseKey, BackgroundProcess)
startInBackgroundWithStopKey cmd = do
  executable <- if cmd.executable == "wasp" then asks (.runConfig.waspCliPath) else return cmd.executable
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
        return BackgroundProcess {processCommand = cmd, process, processGroupId, output}

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

-- | Waits until the process prints the text (to stdout or stderr).
-- Fails the test if that doesn't happen in time or if the process exits first.
waitForOutput :: BackgroundProcess -> T.Text -> Int -> TestM ()
waitForOutput backgroundProcess expectedText seconds = do
  maybeOutcome <-
    liftIO . waitAtMost seconds $
      (outputContainsText >>= check >> return OutputFound)
        `orElse` (processExitedWithAllOutputRead >>= check >> return ProcessExited)
  output <- getBackgroundOutput backgroundProcess
  case maybeOutcome of
    Just OutputFound -> logLine $ "\n==> Found expected output: " ++ show expectedText
    Just ProcessExited -> failTest $ describeFailure backgroundProcess.processCommand ("exited before printing " ++ show expectedText) output
    Nothing -> failTest $ describeFailure backgroundProcess.processCommand ("didn't print " ++ show expectedText ++ " within " ++ show seconds ++ "s") output
  where
    outputContainsText :: STM Bool
    outputContainsText = T.isInfixOf expectedText <$> readCombinedOutput backgroundProcess.output

    processExitedWithAllOutputRead :: STM Bool
    processExitedWithAllOutputRead = (&&) . isJust <$> TP.getExitCodeSTM backgroundProcess.process <*> isFullyRead backgroundProcess.output

data WaitForOutputOutcome = OutputFound | ProcessExited

-- | Stdout and stderr the process printed so far, interleaved.
getBackgroundOutput :: BackgroundProcess -> TestM T.Text
getBackgroundOutput backgroundProcess = liftIO . atomically $ readCombinedOutput backgroundProcess.output

-- | Waits for the process to exit, but at most for the given number of seconds.
-- If it doesn't exit in time, the test fails (and the process is stopped).
waitForExit :: BackgroundProcess -> Int -> TestM ExitCode
waitForExit backgroundProcess seconds = do
  maybeExitCode <- liftIO . waitAtMost seconds $ TP.waitExitCodeSTM backgroundProcess.process
  case maybeExitCode of
    Nothing -> do
      output <- getBackgroundOutput backgroundProcess
      failTest $ describeFailure backgroundProcess.processCommand ("didn't exit within " ++ show seconds ++ "s") output
    Just exitCode -> do
      -- A process can exit before we've read all of its output. We give the
      -- readers a moment to finish. We don't wait longer since children that
      -- outlive the process (e.g. daemons) can keep the pipes open.
      liftIO . void . waitAtMost 5 $ isFullyRead backgroundProcess.output >>= check
      logLine $ "<== " ++ showExitCode exitCode
      return exitCode

-- | Sends the signal to the process group the background process runs in, like
-- a terminal does for Ctrl+C (SIGINT) or when it's closed (SIGHUP).
signalProcessGroupOf :: Signal -> BackgroundProcess -> TestM ()
signalProcessGroupOf sig backgroundProcess = do
  processGroupId <- getProcessGroupId backgroundProcess
  logLine $ "\n==> Sending " ++ show sig ++ " to the process group of: " ++ showCommand backgroundProcess.processCommand
  liftIO $ signalProcessGroup sig processGroupId

-- | The ID of the process group the background process runs in.
getProcessGroupId :: BackgroundProcess -> TestM Int
getProcessGroupId backgroundProcess =
  maybe
    (failTest $ "Command `" ++ showCommand backgroundProcess.processCommand ++ "` exited before its process group ID could be read.")
    return
    backgroundProcess.processGroupId

-- | Interrupts the process group if the process is still running, and kills
-- the group if the process doesn't exit soon after. Finally kills whatever is
-- left of the group, since children can outlive the process.
stopProcess :: BackgroundProcess -> TestM ()
stopProcess backgroundProcess = do
  isRunning <- liftIO $ isNothing <$> TP.getExitCode backgroundProcess.process
  when isRunning $ do
    logLine $ "\n==> Stopping: " ++ showCommand backgroundProcess.processCommand
    signalGroup SIGINT
    maybeExitCode <- liftIO . waitAtMost 10 $ TP.waitExitCodeSTM backgroundProcess.process
    logLine $ maybe "<== didn't stop within 10s, killing it" (("<== " ++) . showExitCode) maybeExitCode
  signalGroup SIGKILL
  -- Only once the process is killed: this sends it SIGTERM, which it can
  -- ignore, and then waits for it to exit without a time limit.
  liftIO $ TP.stopProcess backgroundProcess.process
  where
    signalGroup sig = liftIO . forM_ backgroundProcess.processGroupId $ \processGroupId ->
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
