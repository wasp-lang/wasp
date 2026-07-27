module Wasp.Job.Subprocess.Managed
  ( ManagedSubprocess,
    ProcessTreeDidNotStop (..),
    pollRootExit,
    start,
    stop,
    waitForRootExit,
  )
where

import Control.Concurrent (modifyMVar, newMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception (displayException), SomeException, finally, mask, onException, throwIO, try)
import Control.Monad (unless, void)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (Decoding (Some), decodeUtf8With, streamDecodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Process as P
import System.Timeout (timeout)
import Wasp.Job.Internal (JobOutputEmitter, JobOutputStream (..), emitJobOutputIO)
import qualified Wasp.Job.Subprocess.System as System
import Wasp.Util (secondsToMicroSeconds)

-- Managed subprocesses are Wasp-owned children started now and stopped later.
-- They forward output without ending their owning Job.
data ManagedSubprocess = ManagedSubprocess
  { waitForRootExit :: IO ExitCode,
    pollRootExit :: IO (Maybe ExitCode),
    stop :: IO ()
  }

data ProcessTreeDidNotStop = ProcessTreeDidNotStop
  deriving (Eq, Show)

instance Exception ProcessTreeDidNotStop where
  displayException _ = T.unpack processTreeDidNotStopMessage

start :: P.CreateProcess -> JobOutputEmitter -> IO ManagedSubprocess
start process outputEmitter = mask $ \restore -> do
  processResources@(_, _, _, processHandle) <- P.createProcess $ System.configureManagedSubprocess process
  maybeProcessGroupPid <-
    P.getPid processHandle
      `onException` emergencyCleanUp Nothing processResources
  restore (finishInitialization maybeProcessGroupPid processResources)
    `onException` emergencyCleanUp maybeProcessGroupPid processResources
  where
    finishInitialization maybeProcessGroupPid (maybeStdin, maybeStdout, maybeStderr, processHandle) = do
      rootExitAsync <- Async.async $ P.waitForProcess processHandle
      stdoutAsync <- Async.async $ forwardOutput outputEmitter maybeStdout Stdout
      stderrAsync <- Async.async $ forwardOutput outputEmitter maybeStderr Stderr
      stopWorkerVar <- newMVar Nothing
      let closeHandles = mapM_ closeHandleIfOpen [maybeStdin, maybeStdout, maybeStderr]
      let cleanUpOutput =
            (drainOrCancelOutput stdoutAsync `finally` drainOrCancelOutput stderrAsync)
              `finally` closeHandles
      let performStop = do
            processTreeResult <- try $ System.stopProcessTree processHandle rootExitAsync maybeProcessGroupPid
            outputResult <- try cleanUpOutput
            case (processTreeResult, outputResult) of
              (Left exception, _) -> throwIO (exception :: SomeException)
              (Right False, _) -> do
                emitJobOutputIO outputEmitter Stderr $ processTreeDidNotStopMessage <> "\n"
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
        ManagedSubprocess
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

drainOrCancelOutput :: Async.Async a -> IO ()
drainOrCancelOutput outputAsync = do
  maybeResult <- timeout outputDrainTimeoutMicroseconds $ Async.waitCatch outputAsync
  case maybeResult of
    Nothing -> Async.cancel outputAsync
    Just (Left exception) -> throwIO exception
    Just (Right _) -> return ()

forwardOutput :: JobOutputEmitter -> Maybe Handle -> JobOutputStream -> IO ()
forwardOutput _ Nothing _ = return ()
forwardOutput outputEmitter (Just handle) outputStream =
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
        emitJobOutputIO outputEmitter outputStream output

    chunkSizeInBytes = 4096

closeHandleIfOpen :: Maybe Handle -> IO ()
closeHandleIfOpen Nothing = return ()
closeHandleIfOpen (Just handle) = void (try $ hClose handle :: IO (Either SomeException ()))

emergencyCleanUp :: Maybe P.Pid -> (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
emergencyCleanUp maybeProcessGroupPid (maybeStdin, maybeStdout, maybeStderr, processHandle) = do
  System.killStartedProcessGroup maybeProcessGroupPid
  ignoreExceptions $ P.terminateProcess processHandle
  mapM_ closeHandleIfOpen [maybeStdin, maybeStdout, maybeStderr]
  void $ timeout System.hardStopTimeoutMicroseconds $ ignoreExceptions $ P.waitForProcess processHandle

ignoreExceptions :: IO a -> IO ()
ignoreExceptions action = void (try (void action) :: IO (Either SomeException ()))

outputDrainTimeoutMicroseconds :: Int
outputDrainTimeoutMicroseconds = secondsToMicroSeconds 1

processTreeDidNotStopMessage :: T.Text
processTreeDidNotStopMessage = "Process tree did not stop after a kill signal; it may still be running."
