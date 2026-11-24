{-# LANGUAGE DuplicateRecordFields #-}

module TranscriptTest.Recording
  ( RecordingMode (..),
    ScriptFlavor,
    getAvailableRecordingModes,
    Interaction (..),
    CommandToRecord (..),
    Recording (..),
    RecordedStream (..),
    recordCommand,
  )
where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (IOException, bracket, finally, handle)
import Control.Monad (foldM_, unless, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, isNothing, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import ShellCommands (ShellCommand)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (NoBuffering), Handle, hClose, hFlush, hPutStr, hSetBuffering)
import System.Info (os)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    cleanupProcess,
    createProcess,
    getProcessExitCode,
    interruptProcessGroupOf,
    proc,
    readCreateProcessWithExitCode,
    shell,
    waitForProcess,
  )
import System.Timeout (timeout)

data RecordingMode
  = -- | The command runs in a pseudo-terminal, so it sees a terminal on stdin,
    -- stdout and stderr, like when a user runs it.
    Terminal ScriptFlavor
  | -- | The command's stdin, stdout and stderr are pipes.
    Pipe

instance Show RecordingMode where
  show (Terminal _) = "terminal"
  show Pipe = "pipe"

-- | The `script` tool gives us the pseudo-terminal, and it takes different
-- arguments on different systems.
data ScriptFlavor = UtilLinuxScript | BsdScript

getAvailableRecordingModes :: IO [RecordingMode]
getAvailableRecordingModes = do
  maybeScriptFlavor <- findScriptFlavor
  return $ (Terminal <$> maybeToList maybeScriptFlavor) ++ [Pipe]

findScriptFlavor :: IO (Maybe ScriptFlavor)
findScriptFlavor = do
  isScriptAvailable <- isJust <$> findExecutable "script"
  return $ case os of
    "linux" | isScriptAvailable -> Just UtilLinuxScript
    "darwin" | isScriptAvailable -> Just BsdScript
    _noKnownScript -> Nothing

-- | What the user does while the command runs.
data Interaction
  = -- | Waits until the command prints the text. Each wait looks only at the
    -- output that came after the previous wait's text. A pipe shows no prompts
    -- to wait for, so there the answers are piped ahead and this does nothing.
    WaitForOutput String
  | -- | Waits until the shell condition succeeds.
    WaitUntil ShellCommand
  | TypeLine String
  | PressCtrlC
  | PressCtrlD

data CommandToRecord = CommandToRecord
  { command :: ShellCommand,
    workingDir :: FilePath,
    environment :: [(String, String)]
  }

data Recording = Recording
  { streams :: [RecordedStream],
    -- | The exit code a shell would report.
    exitCode :: Int
  }

data RecordedStream = RecordedStream
  { name :: String,
    content :: BS.ByteString
  }

-- | Runs the command, interacts with it, and records what it prints and how it
-- exits. Stops the command if it's still running once the interactions are
-- done and the time is up.
recordCommand :: RecordingMode -> CommandToRecord -> [Interaction] -> IO Recording
recordCommand mode commandToRecord interactions =
  bracket (startCommand mode commandToRecord) stopCommand $ \runningCommand -> do
    maybeRecording <- timeout recordingTimeoutMicroseconds $ do
      interactWith runningCommand interactions
      processExitCode <- waitForProcess runningCommand.processHandle
      streams <- mapM waitForRecordedStream (outputCollectors runningCommand)
      return Recording {streams, exitCode = exitCodeAsShellReportsIt processExitCode}
    maybe (fail $ "The command didn't finish in time: " ++ commandToRecord.command) return maybeRecording
  where
    recordingTimeoutMicroseconds = 15 * 60 * microsecondsInSecond

data RunningCommand = RunningCommand
  { mode :: RecordingMode,
    commandToRecord :: CommandToRecord,
    processHandle :: ProcessHandle,
    stdinHandle :: Handle,
    stdoutCollector :: OutputCollector,
    stderrCollector :: Maybe OutputCollector
  }

startCommand :: RecordingMode -> CommandToRecord -> IO RunningCommand
startCommand mode commandToRecord = do
  (Just stdinHandle, Just stdoutHandle, maybeStderrHandle, processHandle) <-
    createProcess
      (makeProcess mode)
        { cwd = Just commandToRecord.workingDir,
          env = Just commandToRecord.environment,
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = stderrStream mode,
          create_group = True
        }
  hSetBuffering stdinHandle NoBuffering
  stdoutCollector <- collectOutput (stdoutStreamName mode) stdoutHandle
  stderrCollector <- mapM (collectOutput "stderr") maybeStderrHandle
  return
    RunningCommand
      { mode,
        commandToRecord,
        processHandle,
        stdinHandle,
        stdoutCollector,
        stderrCollector
      }
  where
    -- `exec` makes the command take the shell's place, so that signals reach
    -- it directly and its exit code is the one we record.
    commandTakingShellsPlace = "exec " ++ commandToRecord.command

    makeProcess (Terminal UtilLinuxScript) = proc "script" ["-qefc", commandTakingShellsPlace, "/dev/null"]
    makeProcess (Terminal BsdScript) = proc "script" ["-q", "/dev/null", "/bin/sh", "-c", commandTakingShellsPlace]
    makeProcess Pipe = shell commandTakingShellsPlace

    -- In a terminal, the command's stdout and stderr both end up in the
    -- terminal's output, which `script` prints to its stdout.
    stderrStream (Terminal _) = Inherit
    stderrStream Pipe = CreatePipe

    stdoutStreamName (Terminal _) = "terminal"
    stdoutStreamName Pipe = "stdout"

outputCollectors :: RunningCommand -> [OutputCollector]
outputCollectors runningCommand =
  runningCommand.stdoutCollector : maybeToList runningCommand.stderrCollector

stopCommand :: RunningCommand -> IO ()
stopCommand runningCommand = do
  isStillRunning <- isNothing <$> getProcessExitCode runningCommand.processHandle
  when isStillRunning $ ignoringIOExceptions $ interruptProcessGroupOf runningCommand.processHandle
  cleanupProcess (Just runningCommand.stdinHandle, Nothing, Nothing, runningCommand.processHandle)

interactWith :: RunningCommand -> [Interaction] -> IO ()
interactWith runningCommand interactions = do
  foldM_ (perform runningCommand) startOfOutput interactions
  case runningCommand.mode of
    Terminal _ -> return ()
    -- Like `echo answer | command`: the input ends after the answers.
    Pipe -> closeStdin runningCommand
  where
    startOfOutput = 0

-- | Where in the terminal's output the next 'WaitForOutput' starts looking.
type OutputPosition = Int

perform :: RunningCommand -> OutputPosition -> Interaction -> IO OutputPosition
perform runningCommand position = \case
  WaitForOutput text -> case runningCommand.mode of
    Terminal _ -> waitForOutputAfter runningCommand position text
    Pipe -> return position
  WaitUntil condition -> waitUntil runningCommand condition >> return position
  TypeLine line -> writeToStdin runningCommand (line ++ "\n") >> return position
  PressCtrlC -> pressCtrlC >> return position
  PressCtrlD -> pressCtrlD >> return position
  where
    pressCtrlC = case runningCommand.mode of
      -- The terminal turns the character into SIGINT for everything that runs
      -- in it, the same as when a user presses the keys.
      Terminal _ -> writeToStdin runningCommand "\ETX"
      Pipe -> ignoringIOExceptions $ interruptProcessGroupOf runningCommand.processHandle

    pressCtrlD = case runningCommand.mode of
      Terminal _ -> writeToStdin runningCommand "\EOT"
      Pipe -> closeStdin runningCommand

-- | Returns the position right after the text. Gives up if the command exits
-- without printing it.
waitForOutputAfter :: RunningCommand -> OutputPosition -> String -> IO OutputPosition
waitForOutputAfter runningCommand position text = do
  outputSoFar <- readOutputSoFar runningCommand.stdoutCollector
  let (outputBeforeText, outputFromText) = BS.breakSubstring textBytes (BS.drop position outputSoFar)
      positionAfterText = position + BS.length outputBeforeText + BS.length textBytes
  if not (BS.null outputFromText)
    then return positionAfterText
    else do
      hasExited <- hasCommandExited runningCommand
      if hasExited
        then return position
        else threadDelay pollIntervalMicroseconds >> waitForOutputAfter runningCommand position text
  where
    textBytes = T.encodeUtf8 $ T.pack text
    pollIntervalMicroseconds = microsecondsInSecond `div` 20

-- | Gives up if the command exits before the condition succeeds.
waitUntil :: RunningCommand -> ShellCommand -> IO ()
waitUntil runningCommand condition = do
  (conditionExitCode, _, _) <- readCreateProcessWithExitCode conditionProcess ""
  hasExited <- hasCommandExited runningCommand
  unless (conditionExitCode == ExitSuccess || hasExited) $ do
    threadDelay pollIntervalMicroseconds
    waitUntil runningCommand condition
  where
    conditionProcess =
      (shell condition)
        { cwd = Just runningCommand.commandToRecord.workingDir,
          env = Just runningCommand.commandToRecord.environment
        }
    pollIntervalMicroseconds = microsecondsInSecond

hasCommandExited :: RunningCommand -> IO Bool
hasCommandExited runningCommand = isJust <$> getProcessExitCode runningCommand.processHandle

writeToStdin :: RunningCommand -> String -> IO ()
writeToStdin runningCommand text =
  ignoringIOExceptions $ hPutStr runningCommand.stdinHandle text >> hFlush runningCommand.stdinHandle

closeStdin :: RunningCommand -> IO ()
closeStdin runningCommand = ignoringIOExceptions $ hClose runningCommand.stdinHandle

-- | The command can exit at any point, and then there is nobody to write to
-- or to signal.
ignoringIOExceptions :: IO () -> IO ()
ignoringIOExceptions = handle ignoreIOException
  where
    ignoreIOException :: IOException -> IO ()
    ignoreIOException _ = return ()

-- | Collects what's written to a handle on a separate thread, so the output
-- can be inspected while the command is still running.
data OutputCollector = OutputCollector
  { streamName :: String,
    collectedOutput :: IORef BS.ByteString,
    isDone :: MVar ()
  }

collectOutput :: String -> Handle -> IO OutputCollector
collectOutput streamName outputHandle = do
  collectedOutput <- newIORef BS.empty
  isDone <- newEmptyMVar
  _ <- forkIO $ collectChunksInto collectedOutput `finally` putMVar isDone ()
  return OutputCollector {streamName, collectedOutput, isDone}
  where
    collectChunksInto collectedOutput = do
      chunk <- BS.hGetSome outputHandle chunkSizeBytes
      unless (BS.null chunk) $ do
        atomicModifyIORef' collectedOutput $ \output -> (output <> chunk, ())
        collectChunksInto collectedOutput

    chunkSizeBytes = 4096

readOutputSoFar :: OutputCollector -> IO BS.ByteString
readOutputSoFar outputCollector = readIORef outputCollector.collectedOutput

waitForRecordedStream :: OutputCollector -> IO RecordedStream
waitForRecordedStream outputCollector = do
  readMVar outputCollector.isDone
  content <- readOutputSoFar outputCollector
  return RecordedStream {name = outputCollector.streamName, content}

exitCodeAsShellReportsIt :: ExitCode -> Int
exitCodeAsShellReportsIt ExitSuccess = 0
exitCodeAsShellReportsIt (ExitFailure code)
  | isKilledBySignal = signalExitCodeBase + signalNumber
  | otherwise = code
  where
    isKilledBySignal = code < 0
    signalNumber = negate code
    signalExitCodeBase = 128

microsecondsInSecond :: Int
microsecondsInSecond = 1000000
