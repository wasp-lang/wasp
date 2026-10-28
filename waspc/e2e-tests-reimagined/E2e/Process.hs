{-# LANGUAGE CPP #-}

module E2e.Process
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
    combinedOutput,
    run,
    runExpectingFailure,
    runAllowingFailure,
    startInBackground,
    waitForOutput,
    getBackgroundOutput,
    waitForExit,
    waitForSuccessfulExit,
    interrupt,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    orElse,
    readTVar,
    readTVarIO,
    registerDelay,
    retry,
    writeTVar,
  )
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import E2e.TestM (RunConfig (..), TestEnv (..), TestLog, TestM, failTest, getWorkingDir, logLine, registerCleanup, writeToLog)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (NoBuffering), Handle, hClose, hSetBuffering)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    getPid,
    interruptProcessGroupOf,
    proc,
    waitForProcess,
  )
#if defined(mingw32_HOST_OS)
import System.Process (terminateProcess)
#else
import System.Posix.Signals (sigKILL, signalProcessGroup)
#endif
import Text.Printf (printf)
import UnliftIO.Exception (catchAny)

data Command = Command
  { program :: Program,
    arguments :: [String],
    -- | @Just value@ sets the variable, @Nothing@ removes it.
    environmentOverrides :: [(String, Maybe String)],
    stdinMode :: StdinMode,
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
    stderr :: T.Text
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
      timeoutSeconds = defaultTimeoutSeconds
    }

defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 15 * 60

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

combinedOutput :: ProcessResult -> T.Text
combinedOutput result = result.stdout <> result.stderr

-- | Runs the command and fails the test if it doesn't succeed.
run :: Command -> TestM ProcessResult
run cmd = do
  result <- runAllowingFailure cmd
  case result.exitCode of
    ExitSuccess -> return result
    ExitFailure code -> failTest $ describeFailure cmd ("exited with code " ++ show code) (combinedOutput result)

-- | Runs the command and fails the test if it succeeds.
runExpectingFailure :: Command -> TestM ProcessResult
runExpectingFailure cmd = do
  result <- runAllowingFailure cmd
  case result.exitCode of
    ExitFailure _ -> return result
    ExitSuccess -> failTest $ describeFailure cmd "succeeded, but it was expected to fail" (combinedOutput result)

runAllowingFailure :: Command -> TestM ProcessResult
runAllowingFailure cmd = do
  backgroundProcess <- startInBackground cmd
  exitCode <- waitForExit backgroundProcess
  output <- liftIO $ readOutputs backgroundProcess
  return ProcessResult {exitCode, stdout = fst output, stderr = snd output}

-- | A process that runs while the test continues with other steps.
--
-- It is stopped when the test ends if it is still running by then.
data BackgroundProcess = BackgroundProcess
  { processCommand :: Command,
    processHandle :: ProcessHandle,
    stdoutChunks :: TVar [BS.ByteString],
    stderrChunks :: TVar [BS.ByteString],
    -- | Both output streams have been read to the end.
    outputClosed :: TVar Bool,
    processExitCode :: TVar (Maybe ExitCode),
    stdinHandle :: Maybe Handle
  }

startInBackground :: Command -> TestM BackgroundProcess
startInBackground cmd = do
  executable <- resolveExecutable cmd.program
  workingDir <- getWorkingDir
  environment <- liftIO $ buildEnvironment cmd.environmentOverrides
  testLog <- asks (.testLog)
  logLine $ "\n==> " ++ showCommand cmd ++ "\n    in " ++ workingDir

  (maybeStdin, maybeStdout, maybeStderr, processHandle) <-
    liftIO $
      createProcess
        (proc executable cmd.arguments)
          { cwd = Just workingDir,
            env = environment,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe,
            -- Lets us stop the process together with all of its children.
            create_group = True
          }
  (stdinHandle, stdoutHandle, stderrHandle) <- case (maybeStdin, maybeStdout, maybeStderr) of
    (Just i, Just o, Just e) -> return (i, o, e)
    _ -> failTest "Failed to open process pipes."

  stdoutChunks <- liftIO $ newTVarIO []
  stderrChunks <- liftIO $ newTVarIO []
  stdoutClosed <- liftIO $ newTVarIO False
  stderrClosed <- liftIO $ newTVarIO False
  outputClosed <- liftIO $ newTVarIO False
  processExitCode <- liftIO $ newTVarIO Nothing

  liftIO $ do
    void . forkIO $ collectOutput testLog stdoutHandle stdoutChunks stdoutClosed
    void . forkIO $ collectOutput testLog stderrHandle stderrChunks stderrClosed
    void . forkIO . atomically $
      (&&) <$> readTVar stdoutClosed <*> readTVar stderrClosed >>= check >> writeTVar outputClosed True
    void . forkIO $ waitForProcess processHandle >>= atomically . writeTVar processExitCode . Just

  keptStdinHandle <- liftIO $ case cmd.stdinMode of
    ClosedStdin -> hClose stdinHandle >> return Nothing
    StdinText text -> do
      -- The process may exit without reading its stdin, which closes the pipe.
      BS.hPut stdinHandle (TE.encodeUtf8 text) `catchAny` const (return ())
      hClose stdinHandle `catchAny` const (return ())
      return Nothing
    OpenEmptyStdin -> return (Just stdinHandle)

  let backgroundProcess =
        BackgroundProcess
          { processCommand = cmd,
            processHandle,
            stdoutChunks,
            stderrChunks,
            outputClosed,
            processExitCode,
            stdinHandle = keptStdinHandle
          }
  registerCleanup $ stopProcess backgroundProcess
  return backgroundProcess

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

collectOutput :: TestLog -> Handle -> TVar [BS.ByteString] -> TVar Bool -> IO ()
collectOutput testLog handle chunks closed = do
  hSetBuffering handle NoBuffering
  readChunks `catchAny` const (return ())
  hClose handle `catchAny` const (return ())
  atomically $ writeTVar closed True
  where
    readChunks = do
      chunk <- BS.hGetSome handle 32768
      unless (BS.null chunk) $ do
        atomically $ modifyTVar' chunks (chunk :)
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
    processExitedWithAllOutputRead = (&&) . isJust <$> readTVar backgroundProcess.processExitCode <*> readTVar backgroundProcess.outputClosed

data WaitForOutputOutcome = OutputFound | ProcessExited | TimedOut

-- | Stdout and stderr the process printed so far.
getBackgroundOutput :: BackgroundProcess -> TestM T.Text
getBackgroundOutput = liftIO . atomically . readCombinedOutputSTM

readCombinedOutputSTM :: BackgroundProcess -> STM T.Text
readCombinedOutputSTM backgroundProcess =
  (<>) <$> readChunksSTM backgroundProcess.stdoutChunks <*> readChunksSTM backgroundProcess.stderrChunks

readChunksSTM :: TVar [BS.ByteString] -> STM T.Text
readChunksSTM chunks = decodeChunks <$> readTVar chunks

decodeChunks :: [BS.ByteString] -> T.Text
decodeChunks = TE.decodeUtf8With TE.lenientDecode . BS.concat . reverse

readOutputs :: BackgroundProcess -> IO (T.Text, T.Text)
readOutputs backgroundProcess =
  atomically $ (,) <$> readChunksSTM backgroundProcess.stdoutChunks <*> readChunksSTM backgroundProcess.stderrChunks

-- | Waits for the process to exit, but at most for the command's timeout.
-- If the timeout passes, the process is stopped and the test fails.
waitForExit :: BackgroundProcess -> TestM ExitCode
waitForExit backgroundProcess = do
  startTime <- liftIO getCurrentTime
  timedOut <- liftIO $ registerDelay (backgroundProcess.processCommand.timeoutSeconds * 1000000)
  maybeExitCode <-
    liftIO . atomically $
      (Just <$> (readTVar backgroundProcess.processExitCode >>= maybe retry return))
        `orElse` (readTVar timedOut >>= check >> return Nothing)
  case maybeExitCode of
    Nothing -> do
      liftIO $ stopProcess backgroundProcess
      output <- getBackgroundOutput backgroundProcess
      failTest $ describeFailure backgroundProcess.processCommand ("timed out after " ++ show backgroundProcess.processCommand.timeoutSeconds ++ "s") output
    Just exitCode -> do
      liftIO $ closeKeptStdin backgroundProcess
      liftIO $ waitForOutputToClose backgroundProcess
      endTime <- liftIO getCurrentTime
      logLine $
        "<== "
          ++ showExitCode exitCode
          ++ printf " after %.1fs" (realToFrac (diffUTCTime endTime startTime) :: Double)
      return exitCode

waitForSuccessfulExit :: BackgroundProcess -> TestM ()
waitForSuccessfulExit backgroundProcess =
  waitForExit backgroundProcess >>= \case
    ExitSuccess -> return ()
    ExitFailure code -> do
      output <- getBackgroundOutput backgroundProcess
      failTest $ describeFailure backgroundProcess.processCommand ("exited with code " ++ show code) output

-- | Sends SIGINT to the process and all of its children, like Ctrl+C in a terminal would.
interrupt :: BackgroundProcess -> TestM ()
interrupt backgroundProcess = do
  logLine $ "\n==> Interrupting: " ++ showCommand backgroundProcess.processCommand
  liftIO $ interruptProcessGroupOf backgroundProcess.processHandle `catchAny` const (return ())

-- | Interrupts the process if it's still running, and kills it if it doesn't exit soon after.
-- Finally kills whatever is left of its process group, since children can
-- outlive the process (e.g. Wasp ignores SIGINT while it has a child, so its
-- child may still be running).
stopProcess :: BackgroundProcess -> IO ()
stopProcess backgroundProcess = do
  isRunning <- isNothing <$> readTVarIO backgroundProcess.processExitCode
  when isRunning $ do
    interruptProcessGroupOf backgroundProcess.processHandle `catchAny` const (return ())
    gracePeriodPassed <- registerDelay (10 * 1000000)
    atomically $
      (readTVar backgroundProcess.processExitCode >>= check . isJust)
        `orElse` (readTVar gracePeriodPassed >>= check)
  killProcessGroup backgroundProcess.processHandle
  closeKeptStdin backgroundProcess

killProcessGroup :: ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
killProcessGroup = terminateProcess
#else
killProcessGroup processHandle =
  -- The process was started with 'create_group', so its PID is also its process group ID.
  getPid processHandle >>= mapM_ (\pid -> signalProcessGroup sigKILL pid `catchAny` const (return ()))
#endif

closeKeptStdin :: BackgroundProcess -> IO ()
closeKeptStdin backgroundProcess =
  forM_ backgroundProcess.stdinHandle $ \handle -> hClose handle `catchAny` const (return ())

-- | A process can exit before we've read all of its output. We give the readers
-- a moment to finish. We don't wait longer since children that outlive the
-- process (e.g. daemons) can keep the pipes open.
waitForOutputToClose :: BackgroundProcess -> IO ()
waitForOutputToClose backgroundProcess = do
  gracePeriodPassed <- registerDelay (5 * 1000000)
  atomically $ readTVar backgroundProcess.outputClosed `orElseTrue` readTVar gracePeriodPassed
  -- Gives the log writes of the reader threads a moment to land before the exit line.
  threadDelay 1000
  where
    orElseTrue first second = (first >>= check) `orElse` (second >>= check)

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
showExitCode (ExitFailure code) = "exit code " ++ show code
