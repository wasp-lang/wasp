-- | Running commands as processes, without a shell.
module E2e.Process
  ( Command (..),
    Program (..),
    StdinMode (..),
    ProcessResult (..),
    BackgroundProcess (processGroupId),
    command,
    waspCli,
    nodeModulesBin,
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
  )
where

import Control.Concurrent (forkIO)
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
import Control.Monad.Reader (ask, asks)
import qualified Data.ByteString as BS
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import E2e.Signals (Signal (..), signalProcess, signalProcessGroup)
import E2e.TestM (RunConfig (..), TestEnv (..), TestLog, TestM, failTest, getWorkingDir, logLine, registerCleanup, resolvePath, runTestM, writeToLog)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (searchPathSeparator, (</>))
import System.IO (BufferMode (NoBuffering), Handle, hClose, hSetBuffering)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    getPid,
    proc,
    waitForProcess,
  )
import Text.Printf (printf)
import UnliftIO.Exception (catchAny, mask_)

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
  | -- | A binary of an npm package installed in the working directory. Like
    -- with @npx@, the other binaries there are on its @PATH@.
    NodeModulesBin String

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

-- | We run the package's binary directly instead of through @npx@, which
-- doesn't forward signals correctly in some environments.
nodeModulesBin :: String -> [String] -> Command
nodeModulesBin = makeCommand . NodeModulesBin

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
    processHandle :: ProcessHandle,
    -- | The process was started with 'create_group', so its PID is also its
    -- process group ID.
    processGroupId :: Int,
    startTime :: UTCTime,
    -- | Output of both streams in the order it arrived, newest first.
    outputChunks :: TVar [(OutputStream, BS.ByteString)],
    -- | Both output streams have been read to the end.
    outputClosed :: TVar Bool,
    processExitCode :: TVar (Maybe ExitCode),
    stdinHandle :: Maybe Handle
  }

data OutputStream = Stdout | Stderr

-- | Starts the process in its own process group, so we can stop it together
-- with its children.
startInBackground :: Command -> TestM BackgroundProcess
startInBackground cmd = do
  executable <- resolveExecutable cmd.program
  workingDir <- getWorkingDir
  pathOverride <- case cmd.program of
    NodeModulesBin _ -> do
      binDir <- resolvePath nodeModulesBinDir
      path <- liftIO $ lookupEnv "PATH"
      return [("PATH", Just $ binDir ++ maybe "" (searchPathSeparator :) path)]
    _ -> return []
  environment <- liftIO $ buildEnvironment (pathOverride ++ cmd.environmentOverrides)
  testEnv <- ask
  logLine $ "\n==> " ++ showCommand cmd ++ "\n    in " ++ workingDir

  -- Masked so that the test can't be cancelled between starting the process
  -- and registering its cleanup, which would leave the process running.
  (backgroundProcess, maybeStdin) <- mask_ $ do
    (maybeStdin, maybeStdout, maybeStderr, processHandle) <-
      liftIO $
        createProcess
          (proc executable cmd.arguments)
            { cwd = Just workingDir,
              env = environment,
              std_in = CreatePipe,
              std_out = CreatePipe,
              std_err = CreatePipe,
              create_group = True
            }
    processGroupId <- liftIO $ maybe (fail "The process exited right away.") (return . fromIntegral) =<< getPid processHandle
    startTime <- liftIO getCurrentTime
    outputChunks <- liftIO $ newTVarIO []
    outputClosed <- liftIO $ newTVarIO False
    processExitCode <- liftIO $ newTVarIO Nothing
    let backgroundProcess =
          BackgroundProcess
            { processCommand = cmd,
              processHandle,
              processGroupId,
              startTime,
              outputChunks,
              outputClosed,
              processExitCode,
              stdinHandle = case cmd.stdinMode of
                OpenEmptyStdin -> maybeStdin
                _ -> Nothing
            }
    registerCleanup $ runTestM testEnv $ stopProcess backgroundProcess

    case (maybeStdout, maybeStderr) of
      (Just stdoutHandle, Just stderrHandle) -> liftIO $ do
        stdoutClosed <- newTVarIO False
        stderrClosed <- newTVarIO False
        void . forkIO $ collectOutput testEnv.testLog Stdout stdoutHandle outputChunks stdoutClosed
        void . forkIO $ collectOutput testEnv.testLog Stderr stderrHandle outputChunks stderrClosed
        void . forkIO . atomically $
          (&&) <$> readTVar stdoutClosed <*> readTVar stderrClosed >>= check >> writeTVar outputClosed True
        void . forkIO $ waitForProcess processHandle >>= atomically . writeTVar processExitCode . Just
      _ -> failTest "Failed to open the process's output pipes."
    return (backgroundProcess, maybeStdin)

  -- We write stdin only once the output readers are running. A process that
  -- is blocked writing to a full output pipe doesn't read its stdin, so
  -- writing to it could block us forever.
  forM_ maybeStdin $ \stdinHandle -> liftIO $
    case cmd.stdinMode of
      ClosedStdin -> hClose stdinHandle
      StdinText text -> do
        -- The process may exit without reading its stdin, which closes the pipe.
        BS.hPut stdinHandle (TE.encodeUtf8 text) `catchAny` const (return ())
        hClose stdinHandle `catchAny` const (return ())
      OpenEmptyStdin -> return ()
  return backgroundProcess

resolveExecutable :: Program -> TestM FilePath
resolveExecutable WaspCli = asks (.runConfig.waspCliPath)
resolveExecutable (Executable executable) = return executable
resolveExecutable (NodeModulesBin bin) = resolvePath $ nodeModulesBinDir </> bin

nodeModulesBinDir :: FilePath
nodeModulesBinDir = "node_modules" </> ".bin"

buildEnvironment :: [(String, Maybe String)] -> IO (Maybe [(String, String)])
buildEnvironment [] = return Nothing -- Inherit the test runner's environment.
buildEnvironment overrides = Just . applyOverrides <$> getEnvironment
  where
    applyOverrides environment = foldl applyOverride environment overrides
    applyOverride environment (key, maybeValue) =
      filter ((/= key) . fst) environment ++ maybe [] (\value -> [(key, value)]) maybeValue

collectOutput :: TestLog -> OutputStream -> Handle -> TVar [(OutputStream, BS.ByteString)] -> TVar Bool -> IO ()
collectOutput testLog stream handle chunks closed = do
  hSetBuffering handle NoBuffering
  readChunks `catchAny` const (return ())
  hClose handle `catchAny` const (return ())
  atomically $ writeTVar closed True
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
    processExitedWithAllOutputRead = (&&) . isJust <$> readTVar backgroundProcess.processExitCode <*> readTVar backgroundProcess.outputClosed

data WaitForOutputOutcome = OutputFound | ProcessExited | TimedOut

-- | Stdout and stderr the process printed so far, interleaved.
getBackgroundOutput :: BackgroundProcess -> TestM T.Text
getBackgroundOutput = liftIO . atomically . readCombinedOutputSTM

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
      (Just <$> (readTVar backgroundProcess.processExitCode >>= maybe retry return))
        `orElse` (readTVar timedOut >>= check >> return Nothing)
  case maybeExitCode of
    Nothing -> do
      stopProcess backgroundProcess
      output <- getBackgroundOutput backgroundProcess
      failTest $ describeFailure backgroundProcess.processCommand ("didn't exit within " ++ show seconds ++ "s") output
    Just exitCode -> do
      liftIO $ closeKeptStdin backgroundProcess
      liftIO $ waitForOutputToClose backgroundProcess
      endTime <- liftIO getCurrentTime
      logLine $
        "\n<== "
          ++ showExitCode exitCode
          ++ printf " after %.1fs" (realToFrac (diffUTCTime endTime backgroundProcess.startTime) :: Double)
      return exitCode

-- | Sends SIGINT to the process and all of its children, like Ctrl+C in a terminal would.
interrupt :: BackgroundProcess -> TestM ()
interrupt = signalProcessGroupOf SIGINT

-- | Sends the signal to the process group the background process runs in.
signalProcessGroupOf :: Signal -> BackgroundProcess -> TestM ()
signalProcessGroupOf sig backgroundProcess = do
  logLine $ "\n==> Sending " ++ show sig ++ " to the process group of: " ++ showCommand backgroundProcess.processCommand
  liftIO $ signalProcessGroup sig backgroundProcess.processGroupId

-- | Sends the signal to the process with the PID.
signal :: Signal -> Int -> TestM ()
signal sig pid = do
  logLine $ "\n==> Sending " ++ show sig ++ " to PID " ++ show pid
  liftIO $ signalProcess sig pid

-- | Interrupts the process group if the process is still running, and kills
-- the group if the process doesn't exit soon after. Finally kills whatever is
-- left of the group, since children can outlive the process.
stopProcess :: BackgroundProcess -> TestM ()
stopProcess backgroundProcess = do
  isRunning <- liftIO $ isNothing <$> readTVarIO backgroundProcess.processExitCode
  when isRunning $ do
    logLine $ "\n==> Stopping: " ++ showCommand backgroundProcess.processCommand
    signalGroup SIGINT
    gracePeriodPassed <- liftIO $ registerDelay (10 * 1000000)
    maybeExitCode <-
      liftIO . atomically $
        (Just <$> (readTVar backgroundProcess.processExitCode >>= maybe retry return))
          `orElse` (readTVar gracePeriodPassed >>= check >> return Nothing)
    logLine $ maybe "\n<== didn't stop within 10s, killing it" (("\n<== " ++) . showExitCode) maybeExitCode
  signalGroup SIGKILL
  liftIO $ closeKeptStdin backgroundProcess
  where
    signalGroup sig = liftIO $ signalProcessGroup sig backgroundProcess.processGroupId `catchAny` const (return ())

closeKeptStdin :: BackgroundProcess -> IO ()
closeKeptStdin backgroundProcess =
  forM_ backgroundProcess.stdinHandle $ \handle -> hClose handle `catchAny` const (return ())

-- | A process can exit before we've read all of its output. We give the readers
-- a moment to finish. We don't wait longer since children that outlive the
-- process (e.g. daemons) can keep the pipes open.
waitForOutputToClose :: BackgroundProcess -> IO ()
waitForOutputToClose backgroundProcess = do
  gracePeriodPassed <- registerDelay (5 * 1000000)
  atomically $
    (readTVar backgroundProcess.outputClosed >>= check)
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
      NodeModulesBin bin -> "node_modules/.bin/" ++ bin
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
