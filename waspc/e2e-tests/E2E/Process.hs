{-# LANGUAGE DataKinds #-}

-- | Running commands as processes, without a shell.
module E2E.Process
  ( Command (..),
    Program (..),
    StdinMode (..),
    ProcessResult (..),
    BackgroundProcess,
    command,
    waspCli,
    withEnv,
    withoutEnv,
    withStdinText,
    withOpenEmptyStdin,
    withTimeout,
    run,
    runExpectingFailure,
    runAllowingFailure,
    startInBackground,
    waitForOutput,
    getBackgroundOutput,
    waitForExit,
    interrupt,
    signal,
    signalProcessGroupOf,
    getProcessGroupId,
  )
where

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    orElse,
    readTVar,
    registerDelay,
  )
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Trans.Resource (ReleaseKey, release)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import E2E.Signals (Signal (..), signalProcess, signalProcessGroup)
import E2E.TestM (RunConfig (..), TestEnv (..), TestLog, TestM, allocateWithCleanup, failTest, getWorkingDir, logLine, runTestM, writeToLog)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (NoBuffering), Handle, hClose, hSetBuffering)
import System.Process (getPid)
import qualified System.Process.Typed as TP
import Text.Printf (printf)
import UnliftIO.Exception (catchAny)

data Command = Command
  { program :: Program,
    arguments :: [String],
    -- | @Just value@ sets the variable, @Nothing@ removes it.
    environmentOverrides :: [(String, Maybe String)],
    stdinMode :: StdinMode,
    -- | How long 'run' and its variants wait for the command to exit.
    timeoutSeconds :: Int
  }

data Program
  = -- | The Wasp CLI under test.
    WaspCli
  | Executable String

data StdinMode
  = -- | The process reads EOF from stdin right away.
    ClosedStdin
  | -- | The process reads the text and then EOF.
    StdinText T.Text
  | -- | Stdin stays open, without any data, until the process exits.
    OpenEmptyStdin

data ProcessResult = ProcessResult
  { exitCode :: ExitCode,
    stdout :: T.Text,
    stderr :: T.Text,
    -- | Stdout and stderr interleaved in the order they arrived, like @2>&1@.
    combinedOutput :: T.Text
  }

command :: String -> [String] -> Command
command executable = makeCommand (Executable executable)

waspCli :: [String] -> Command
waspCli = makeCommand WaspCli

makeCommand :: Program -> [String] -> Command
makeCommand commandProgram commandArguments =
  Command
    { program = commandProgram,
      arguments = commandArguments,
      environmentOverrides = [],
      stdinMode = ClosedStdin,
      timeoutSeconds = 15 * 60
    }

withEnv :: [(String, String)] -> Command -> Command
withEnv variables cmd = cmd {environmentOverrides = cmd.environmentOverrides ++ [(key, Just value) | (key, value) <- variables]}

withoutEnv :: [String] -> Command -> Command
withoutEnv keys cmd = cmd {environmentOverrides = cmd.environmentOverrides ++ [(key, Nothing) | key <- keys]}

withStdinText :: T.Text -> Command -> Command
withStdinText text cmd = cmd {stdinMode = StdinText text}

withOpenEmptyStdin :: Command -> Command
withOpenEmptyStdin cmd = cmd {stdinMode = OpenEmptyStdin}

withTimeout :: Int -> Command -> Command
withTimeout seconds cmd = cmd {timeoutSeconds = seconds}

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
  backgroundProcess <- startInBackground cmd
  exitCode <- waitForExit backgroundProcess cmd.timeoutSeconds
  -- Stops what the command left running right away, instead of when the test
  -- ends. By then, its process group ID could belong to another group.
  release backgroundProcess.stopKey
  liftIO . atomically $ do
    chunks <- reverse <$> readTVar backgroundProcess.outputChunks
    return
      ProcessResult
        { exitCode,
          stdout = decodeChunks [chunk | (Stdout, chunk) <- chunks],
          stderr = decodeChunks [chunk | (Stderr, chunk) <- chunks],
          combinedOutput = decodeChunks $ map snd chunks
        }

-- | A process that runs while the test continues with other steps.
--
-- If it's still running when the test ends, it's stopped (see 'stopProcess').
data BackgroundProcess = BackgroundProcess
  { processCommand :: Command,
    process :: RunningProcess,
    -- | The process was started in its own group, so its PID is also its
    -- process group ID. 'Nothing' if it exited before we could read its PID.
    processGroupId :: Maybe Int,
    startTime :: UTCTime,
    -- | Output of both streams in the order it arrived, newest first.
    outputChunks :: TVar [(OutputStream, BS.ByteString)],
    -- | Releasing it stops the process. Otherwise, that happens when the test ends.
    stopKey :: ReleaseKey
  }

-- | Stdin is kept only with 'OpenEmptyStdin'. Stdout and stderr come with
-- the threads that read them.
type RunningProcess = TP.Process (Maybe Handle) (Async ()) (Async ())

data OutputStream = Stdout | Stderr

-- | Starts the process in its own process group, so we can stop it together
-- with its children.
startInBackground :: Command -> TestM BackgroundProcess
startInBackground cmd = do
  executable <- resolveExecutable cmd.program
  workingDir <- getWorkingDir
  environment <- liftIO $ buildEnvironment cmd.environmentOverrides
  testEnv <- ask
  outputChunks <- liftIO $ newTVarIO []
  logLine $ "\n==> " ++ showCommand cmd ++ "\n    in " ++ workingDir

  let processConfig =
        TP.setCreateGroup True
          . TP.setWorkingDir workingDir
          . maybe id TP.setEnv environment
          . TP.setStdin (stdinSpec cmd.stdinMode)
          . TP.setStdout (outputSpec testEnv.testLog outputChunks Stdout)
          . TP.setStderr (outputSpec testEnv.testLog outputChunks Stderr)
          $ TP.proc executable cmd.arguments
      start = do
        process <- TP.startProcess processConfig
        processGroupId <- fmap fromIntegral <$> getPid (TP.unsafeProcessHandle process)
        return (process, processGroupId)

  startTime <- liftIO getCurrentTime
  (stopKey, (process, processGroupId)) <-
    allocateWithCleanup start $ \(process, processGroupId) ->
      runTestM testEnv $ stopProcess cmd process processGroupId
  return BackgroundProcess {processCommand = cmd, process, processGroupId, startTime, outputChunks, stopKey}

stdinSpec :: StdinMode -> TP.StreamSpec 'TP.STInput (Maybe Handle)
stdinSpec = \case
  ClosedStdin -> Nothing <$ TP.byteStringInput BSL.empty
  -- Written from another thread, since a process that is blocked writing to
  -- a full output pipe doesn't read its stdin.
  StdinText text -> Nothing <$ TP.byteStringInput (BSL.fromStrict $ TE.encodeUtf8 text)
  OpenEmptyStdin -> Just <$> TP.createPipe

-- | Reads the output in a thread, which closes the pipe once it's done.
-- Closing it from elsewhere would wait for a read in progress, which lasts as
-- long as any children that outlive the process keep the pipe open.
outputSpec :: TestLog -> TVar [(OutputStream, BS.ByteString)] -> OutputStream -> TP.StreamSpec streamType (Async ())
outputSpec testLog chunks stream = TP.mkPipeStreamSpec $ \_ handle -> do
  reader <- Async.asyncWithUnmask $ \unmask -> unmask $ collectOutput testLog stream handle chunks
  return (reader, return ())

resolveExecutable :: Program -> TestM FilePath
resolveExecutable WaspCli = asks (.runConfig.waspCliPath)
resolveExecutable (Executable executable) = return executable

buildEnvironment :: [(String, Maybe String)] -> IO (Maybe [(String, String)])
buildEnvironment [] = return Nothing -- Inherit the test runner's environment.
buildEnvironment overrides = Just . applyOverrides <$> getEnvironment
  where
    applyOverrides environment = foldl applyOverride environment overrides
    applyOverride environment (key, maybeValue) =
      filter ((/= key) . fst) environment ++ maybe [] (\value -> [(key, value)]) maybeValue

collectOutput :: TestLog -> OutputStream -> Handle -> TVar [(OutputStream, BS.ByteString)] -> IO ()
collectOutput testLog stream handle chunks = do
  hSetBuffering handle NoBuffering
  readChunks `catchAny` const (return ())
  hClose handle `catchAny` const (return ())
  where
    readChunks = do
      chunk <- BS.hGetSome handle 32768
      unless (BS.null chunk) $ do
        atomically $ modifyTVar' chunks ((stream, chunk) :)
        writeToLog testLog chunk
        readChunks

-- | Waits until the process prints the text (to stdout or stderr).
-- Fails the test if that doesn't happen in time or if the process exits first.
waitForOutput :: BackgroundProcess -> T.Text -> Int -> TestM ()
waitForOutput backgroundProcess expectedText seconds = do
  timedOut <- liftIO $ registerDelay (seconds * 1000000)
  outcome <-
    liftIO . atomically $
      (outputContainsText >>= check >> return OutputFound)
        `orElse` (processExitedWithAllOutputRead >>= check >> return ProcessExited)
        `orElse` (readTVar timedOut >>= check >> return TimedOut)
  output <- getBackgroundOutput backgroundProcess
  case outcome of
    OutputFound -> logLine $ "\n==> Found expected output: " ++ show expectedText
    ProcessExited -> failTest $ describeFailure backgroundProcess.processCommand ("exited before printing " ++ show expectedText) output
    TimedOut -> failTest $ describeFailure backgroundProcess.processCommand ("didn't print " ++ show expectedText ++ " within " ++ show seconds ++ "s") output
  where
    outputContainsText :: STM Bool
    outputContainsText = T.isInfixOf expectedText <$> readCombinedOutputSTM backgroundProcess

    processExitedWithAllOutputRead :: STM Bool
    processExitedWithAllOutputRead = (&&) . isJust <$> TP.getExitCodeSTM backgroundProcess.process <*> isOutputClosedSTM backgroundProcess.process

data WaitForOutputOutcome = OutputFound | ProcessExited | TimedOut

-- | Stdout and stderr the process printed so far, interleaved.
getBackgroundOutput :: BackgroundProcess -> TestM T.Text
getBackgroundOutput = liftIO . atomically . readCombinedOutputSTM

-- | Both output streams have been read to the end.
isOutputClosedSTM :: RunningProcess -> STM Bool
isOutputClosedSTM process = (&&) <$> isDone (TP.getStdout process) <*> isDone (TP.getStderr process)
  where
    isDone = fmap isJust . Async.pollSTM

readCombinedOutputSTM :: BackgroundProcess -> STM T.Text
readCombinedOutputSTM backgroundProcess = decodeChunks . map snd . reverse <$> readTVar backgroundProcess.outputChunks

decodeChunks :: [BS.ByteString] -> T.Text
decodeChunks = TE.decodeUtf8With TE.lenientDecode . BS.concat

-- | Waits for the process to exit, but at most for the given number of seconds.
-- If it doesn't exit in time, the process is stopped and the test fails.
waitForExit :: BackgroundProcess -> Int -> TestM ExitCode
waitForExit backgroundProcess seconds = do
  timedOut <- liftIO $ registerDelay (seconds * 1000000)
  maybeExitCode <-
    liftIO . atomically $
      (Just <$> TP.waitExitCodeSTM backgroundProcess.process)
        `orElse` (readTVar timedOut >>= check >> return Nothing)
  case maybeExitCode of
    Nothing -> do
      release backgroundProcess.stopKey
      output <- getBackgroundOutput backgroundProcess
      failTest $ describeFailure backgroundProcess.processCommand ("didn't exit within " ++ show seconds ++ "s") output
    Just exitCode -> do
      liftIO $ closeKeptStdin backgroundProcess.process
      liftIO $ waitForOutputToClose backgroundProcess.process
      endTime <- liftIO getCurrentTime
      logLine $
        "<== "
          ++ showExitCode exitCode
          ++ printf " after %.1fs" (realToFrac (diffUTCTime endTime backgroundProcess.startTime) :: Double)
      return exitCode

-- | Sends SIGINT to the process and all of its children, like Ctrl+C in a terminal would.
interrupt :: BackgroundProcess -> TestM ()
interrupt = signalProcessGroupOf SIGINT

-- | Sends the signal to the process group the background process runs in.
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

-- | Sends the signal to the process with the PID.
signal :: Signal -> Int -> TestM ()
signal sig pid = do
  logLine $ "\n==> Sending " ++ show sig ++ " to PID " ++ show pid
  liftIO $ signalProcess sig pid

-- | Interrupts the process group if the process is still running, and kills
-- the group if the process doesn't exit soon after. Finally kills whatever is
-- left of the group, since children can outlive the process.
stopProcess :: Command -> RunningProcess -> Maybe Int -> TestM ()
stopProcess cmd process maybeProcessGroupId = do
  isRunning <- liftIO $ isNothing <$> TP.getExitCode process
  when isRunning $ do
    logLine $ "\n==> Stopping: " ++ showCommand cmd
    signalGroup SIGINT
    gracePeriodPassed <- liftIO $ registerDelay (10 * 1000000)
    maybeExitCode <-
      liftIO . atomically $
        (Just <$> TP.waitExitCodeSTM process)
          `orElse` (readTVar gracePeriodPassed >>= check >> return Nothing)
    logLine $ maybe "<== didn't stop within 10s, killing it" (("<== " ++) . showExitCode) maybeExitCode
  signalGroup SIGKILL
  -- Only once the process is killed: this sends it SIGTERM, which it can
  -- ignore, and then waits for it to exit without a time limit.
  liftIO $ TP.stopProcess process
  where
    signalGroup sig = liftIO . forM_ maybeProcessGroupId $ \processGroupId ->
      signalProcessGroup sig processGroupId `catchAny` const (return ())

closeKeptStdin :: RunningProcess -> IO ()
closeKeptStdin process =
  forM_ (TP.getStdin process) $ \handle -> hClose handle `catchAny` const (return ())

-- | A process can exit before we've read all of its output. We give the readers
-- a moment to finish. We don't wait longer since children that outlive the
-- process (e.g. daemons) can keep the pipes open.
waitForOutputToClose :: RunningProcess -> IO ()
waitForOutputToClose process = do
  gracePeriodPassed <- registerDelay (5 * 1000000)
  atomically $
    (isOutputClosedSTM process >>= check)
      `orElse` (readTVar gracePeriodPassed >>= check)

describeFailure :: Command -> String -> T.Text -> String
describeFailure cmd problem output =
  unlines $
    ["Command `" ++ showCommand cmd ++ "` " ++ problem ++ ".", "Last lines of its output:"]
      ++ map (("  " ++) . T.unpack) (lastLines 30 output)
  where
    lastLines count = reverse . take count . reverse . T.lines

showCommand :: Command -> String
showCommand cmd = unwords $ envPrefix ++ map quoteIfNeeded (programName : cmd.arguments)
  where
    programName = case cmd.program of
      WaspCli -> "wasp"
      Executable executable -> executable
    envPrefix = [key ++ "=" ++ quoteIfNeeded value | (key, Just value) <- cmd.environmentOverrides]
    quoteIfNeeded word
      | null word || any (`elem` (" \"'\n" :: String)) word = show word
      | otherwise = word

showExitCode :: ExitCode -> String
showExitCode ExitSuccess = "exit code 0"
showExitCode (ExitFailure code)
  -- That's how 'waitForProcess' reports processes stopped by a signal.
  | code < 0 = "stopped by signal " ++ show (negate code)
  | otherwise = "exit code " ++ show code
