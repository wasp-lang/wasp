{-# LANGUAGE CPP #-}

module Wasp.Job.Process.LongRunning
  ( LongRunningProcess,
    ProcessTreeDidNotStop (..),
    pollRootExit,
    runAsJob,
    start,
    stop,
    waitForRootExit,
  )
where

import Control.Concurrent (Chan, modifyMVar, newMVar, threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception (displayException), SomeException, bracket, finally, mask, onException, throwIO, try)
import Control.Monad (unless, void, when)
import Control.Monad.Extra (anyM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (Decoding (Some), decodeUtf8With, streamDecodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Process as P
import System.Timeout (timeout)
import qualified Wasp.Job as J
import Wasp.Util (secondsToMicroSeconds)

#if !mingw32_HOST_OS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError, isPermissionError, tryIOError)
import qualified System.Posix.Signals as Signals
import Text.Read (readMaybe)
#endif

-- Long-running processes are Wasp-owned children started now and stopped later.
-- They forward output to the job channel, but don't emit JobExit.
data LongRunningProcess = LongRunningProcess
  { waitForRootExit :: IO ExitCode,
    pollRootExit :: IO (Maybe ExitCode),
    stop :: IO ()
  }

data ProcessTreeDidNotStop = ProcessTreeDidNotStop
  deriving (Eq, Show)

instance Exception ProcessTreeDidNotStop where
  displayException _ = T.unpack processTreeDidNotStopMessage

runAsJob :: P.CreateProcess -> J.JobType -> J.Job
runAsJob process jobType =
  J.makeJob jobType $ \chan ->
    bracket
      (start process jobType chan)
      stop
      waitForRootExit

start :: P.CreateProcess -> J.JobType -> Chan J.JobMessage -> IO LongRunningProcess
start process jobType chan = mask $ \restore -> do
  processResources@(_, _, _, processHandle) <- P.createProcess $ configureLongRunningProcess process
  maybeProcessGroupPid <-
    P.getPid processHandle
      `onException` emergencyCleanUp Nothing processResources
  restore (finishInitialization maybeProcessGroupPid processResources)
    `onException` emergencyCleanUp maybeProcessGroupPid processResources
  where
    finishInitialization maybeProcessGroupPid (maybeStdin, maybeStdout, maybeStderr, processHandle) = do
      rootExitAsync <- Async.async $ P.waitForProcess processHandle
      stdoutAsync <- Async.async $ forwardOutput chan jobType maybeStdout J.Stdout
      stderrAsync <- Async.async $ forwardOutput chan jobType maybeStderr J.Stderr
      stopWorkerVar <- newMVar Nothing
      let closeHandles = mapM_ closeHandleIfOpen [maybeStdin, maybeStdout, maybeStderr]
      let cleanUpOutput =
            (drainOrCancelOutput stdoutAsync `finally` drainOrCancelOutput stderrAsync)
              `finally` closeHandles
      let performStop = do
            processTreeResult <- try $ stopProcessTree processHandle rootExitAsync maybeProcessGroupPid
            outputResult <- try cleanUpOutput
            case (processTreeResult, outputResult) of
              (Left exception, _) -> throwIO (exception :: SomeException)
              (Right False, _) -> do
                J.writeJobOutput jobType J.Stderr (processTreeDidNotStopMessage <> "\n") chan
                throwIO ProcessTreeDidNotStop
              (Right True, Left exception) -> throwIO (exception :: SomeException)
              (Right True, Right ()) -> return ()
      let stopOnce = mask $ \restoreStop -> do
            stopWorker <-
              modifyMVar stopWorkerVar $ \maybeStopWorker ->
                case maybeStopWorker of
                  Just existingStopWorker -> return (maybeStopWorker, existingStopWorker)
                  Nothing -> do
                    newStopWorker <- Async.async $ restoreStop performStop
                    return (Just newStopWorker, newStopWorker)
            restoreStop $ Async.wait stopWorker
      return $
        LongRunningProcess
          { waitForRootExit = Async.wait rootExitAsync,
            pollRootExit = pollAsync rootExitAsync,
            stop = stopOnce
          }

pollAsync :: Async.Async a -> IO (Maybe a)
pollAsync action = do
  maybeResult <- Async.poll action
  case maybeResult of
    Nothing -> return Nothing
    Just (Left exception) -> throwIO exception
    Just (Right result) -> return $ Just result

configureLongRunningProcess :: P.CreateProcess -> P.CreateProcess
configureLongRunningProcess process =
  process
    { P.create_group = not isWindows,
      P.use_process_jobs = isWindows,
      -- Isolated process groups cannot safely read the foreground terminal.
      -- Long-running jobs, including Vite, are intentionally noninteractive.
      P.std_in = P.NoStream,
      P.std_out = P.CreatePipe,
      P.std_err = P.CreatePipe
    }

drainOrCancelOutput :: Async.Async a -> IO ()
drainOrCancelOutput outputAsync = do
  maybeResult <- timeout outputDrainTimeoutMicroseconds $ Async.waitCatch outputAsync
  case maybeResult of
    Nothing -> Async.cancel outputAsync
    Just (Left exception) -> throwIO exception
    Just (Right _) -> return ()

forwardOutput :: Chan J.JobMessage -> J.JobType -> Maybe Handle -> J.JobOutputType -> IO ()
forwardOutput _ _ Nothing _ = return ()
forwardOutput chan jobType (Just handle) outputType =
  -- Chunks can split a multi-byte UTF-8 sequence, so decoding must carry
  -- partial sequences over into the next chunk.
  forwardChunks $ streamDecodeUtf8With lenientDecode
  where
    forwardChunks decodeChunk = do
      chunk <- BS.hGetSome handle chunkSizeInBytes
      let Some output undecoded decodeNextChunk = decodeChunk chunk
      emitOutput output
      if BS.null chunk
        then emitOutput $ decodeUtf8With lenientDecode undecoded
        else forwardChunks decodeNextChunk

    emitOutput output =
      unless (T.null output) $
        J.writeJobOutput jobType outputType output chan

    chunkSizeInBytes = 4096

closeHandleIfOpen :: Maybe Handle -> IO ()
closeHandleIfOpen Nothing = return ()
closeHandleIfOpen (Just handle) = void (try $ hClose handle :: IO (Either SomeException ()))

emergencyCleanUp :: Maybe P.Pid -> (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
emergencyCleanUp maybeProcessGroupPid (maybeStdin, maybeStdout, maybeStderr, processHandle) = do
  killStartedProcessGroup maybeProcessGroupPid
  ignoreExceptions $ P.terminateProcess processHandle
  mapM_ closeHandleIfOpen [maybeStdin, maybeStdout, maybeStderr]
  void $ timeout hardStopTimeoutMicroseconds $ ignoreExceptions $ P.waitForProcess processHandle

killStartedProcessGroup :: Maybe P.Pid -> IO ()
#if mingw32_HOST_OS
killStartedProcessGroup _ = return ()
#else
killStartedProcessGroup Nothing = return ()
killStartedProcessGroup (Just processGroupPid) =
  ignoreExceptions $ signalProcessGroupIfAlive Signals.sigKILL processGroupPid
#endif

ignoreExceptions :: IO a -> IO ()
ignoreExceptions action = void (try (void action) :: IO (Either SomeException ()))

stopProcessTree :: P.ProcessHandle -> Async.Async ExitCode -> Maybe P.Pid -> IO Bool
#if mingw32_HOST_OS
stopProcessTree processHandle rootExitAsync _ = do
  void (try (P.terminateProcess processHandle) :: IO (Either SomeException ()))
  waitForAsync rootExitAsync hardStopTimeoutMicroseconds
#else
stopProcessTree processHandle rootExitAsync maybeProcessGroupPid =
  case maybeProcessGroupPid of
    Nothing -> stopRootProcess processHandle rootExitAsync
    Just processGroupPid -> do
      signalProcessGroupIfAlive Signals.sigINT processGroupPid
      stoppedGracefully <- waitForProcessGroupExit processGroupPid gracefulStopTimeoutMicroseconds
      if stoppedGracefully
        then return True
        else do
          signalProcessGroupIfAlive Signals.sigKILL processGroupPid
          waitForProcessGroupExit processGroupPid hardStopTimeoutMicroseconds
#endif

stopRootProcess :: P.ProcessHandle -> Async.Async ExitCode -> IO Bool
stopRootProcess processHandle rootExitAsync = do
  void (try (P.interruptProcessGroupOf processHandle) :: IO (Either SomeException ()))
  stoppedGracefully <- waitForAsync rootExitAsync gracefulStopTimeoutMicroseconds
  if stoppedGracefully
    then return True
    else do
      void (try (P.terminateProcess processHandle) :: IO (Either SomeException ()))
      waitForAsync rootExitAsync hardStopTimeoutMicroseconds

waitForAsync :: Async.Async a -> Int -> IO Bool
waitForAsync action timeoutMicroseconds = do
  maybeResult <- timeout timeoutMicroseconds $ Async.waitCatch action
  case maybeResult of
    Nothing -> return False
    Just (Left exception) -> throwIO exception
    Just (Right _) -> return True

#if !mingw32_HOST_OS
signalProcessGroupIfAlive :: Signals.Signal -> P.Pid -> IO ()
signalProcessGroupIfAlive signal processGroupPid = do
  isAlive <- isProcessGroupAlive processGroupPid
  when isAlive $ ignoreDeadProcessGroup $ Signals.signalProcessGroup signal processGroupPid

waitForProcessGroupExit :: P.Pid -> Int -> IO Bool
waitForProcessGroupExit processGroupPid = waitForCondition $ not <$> isProcessGroupAlive processGroupPid

isProcessGroupAlive :: P.Pid -> IO Bool
isProcessGroupAlive processGroupPid = do
  processGroupExists <-
    (Signals.signalProcessGroup Signals.nullSignal processGroupPid >> return True)
      `catchIOError` \ioErr ->
        if isProcessGroupGoneError ioErr
          then return False
          else ioError ioErr
  if not processGroupExists || not isLinux
    then return processGroupExists
    else do
      maybeHasLiveMember <- linuxProcessGroupHasLiveMember processGroupPid
      case maybeHasLiveMember of
        Nothing -> return True
        Just True -> return True
        Just False -> do
          -- A group member can fork while the first /proc snapshot is being
          -- read. Only call a zombie-only group quiescent after two scans.
          threadDelay linuxZombieConfirmationMicroseconds
          fromMaybe True <$> linuxProcessGroupHasLiveMember processGroupPid

linuxProcessGroupHasLiveMember :: P.Pid -> IO (Maybe Bool)
linuxProcessGroupHasLiveMember processGroupPid = do
  procEntriesResult <- tryIOError $ listDirectory "/proc"
  case procEntriesResult of
    Left _ -> return Nothing
    Right procEntries -> Just <$> anyM isLiveGroupMember procEntries
  where
    processGroupId = show processGroupPid

    isLiveGroupMember procEntry =
      case readMaybe procEntry :: Maybe Int of
        Nothing -> return False
        Just _ -> do
          maybeProcessStateAndGroup <-
            (parseLinuxProcStat <$> BS.readFile ("/proc" </> procEntry </> "stat"))
              `catchIOError` const (return Nothing)
          return $ case maybeProcessStateAndGroup of
            Just (processState, memberProcessGroupId) ->
              memberProcessGroupId == processGroupId
                && processState /= "Z"
                && processState /= "X"
            Nothing -> False

parseLinuxProcStat :: BS.ByteString -> Maybe (String, String)
parseLinuxProcStat procStat =
  case break (== ')') $ reverse $ BSC.unpack procStat of
    (_, []) -> Nothing
    (reversedFieldsAfterCommand, _ : _) ->
      case words $ reverse reversedFieldsAfterCommand of
        processState : _parentPid : processGroupId : _ -> Just (processState, processGroupId)
        _ -> Nothing

ignoreDeadProcessGroup :: IO () -> IO ()
ignoreDeadProcessGroup action =
  action `catchIOError` \ioErr ->
    unless (isProcessGroupGoneError ioErr) $ ioError ioErr

-- ESRCH means the group is fully reaped, on every platform. Signalling a
-- group whose members are all zombies succeeds on Linux (covered by the
-- /proc scan above) but fails with EPERM on macOS: its kernel skips zombies
-- and reports "no one to signal" as a permission error. We only signal
-- groups we created, so EPERM never hides a live group.
isProcessGroupGoneError :: IOError -> Bool
isProcessGroupGoneError ioErr =
  isDoesNotExistError ioErr || isPermissionError ioErr
#endif

waitForCondition :: IO Bool -> Int -> IO Bool
waitForCondition condition = go
  where
    go remainingMicroseconds
      | remainingMicroseconds <= 0 = return False
      | otherwise = do
          conditionMet <- condition
          if conditionMet
            then return True
            else do
              threadDelay pollIntervalMicroseconds
              go $ remainingMicroseconds - pollIntervalMicroseconds

gracefulStopTimeoutMicroseconds :: Int
gracefulStopTimeoutMicroseconds = secondsToMicroSeconds 0.25

hardStopTimeoutMicroseconds :: Int
hardStopTimeoutMicroseconds = secondsToMicroSeconds 5

outputDrainTimeoutMicroseconds :: Int
outputDrainTimeoutMicroseconds = secondsToMicroSeconds 1

processTreeDidNotStopMessage :: T.Text
processTreeDidNotStopMessage = "Process tree did not stop after a kill signal; it may still be running."

pollIntervalMicroseconds :: Int
pollIntervalMicroseconds = secondsToMicroSeconds 0.1

#if !mingw32_HOST_OS
linuxZombieConfirmationMicroseconds :: Int
linuxZombieConfirmationMicroseconds = secondsToMicroSeconds 0.01
#endif

isWindows :: Bool
#if mingw32_HOST_OS
isWindows = True
#else
isWindows = False
#endif

#if !mingw32_HOST_OS
isLinux :: Bool
#if linux_HOST_OS
isLinux = True
#else
isLinux = False
#endif
#endif
