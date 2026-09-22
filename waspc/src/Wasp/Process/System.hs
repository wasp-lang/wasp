{-# LANGUAGE CPP #-}

module Wasp.Process.System
  ( configureIsolatedProcess,
    hardStopTimeoutMicroseconds,
    killStartedProcessGroup,
    stopProcessGroup,
  )
where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, void)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode)
import qualified System.Process as P
import System.Timeout (timeout)
import Wasp.Util (secondsToMicroSeconds)

#if !mingw32_HOST_OS
import System.IO.Error (catchIOError, isDoesNotExistError, isPermissionError)
import qualified System.Posix.Signals as Signals
#endif

-- TODO: Detect when the command exits on Windows even if a child is still
-- running. waitForProcess waits for the whole job.
-- https://github.com/wasp-lang/wasp/issues/4894
configureIsolatedProcess :: P.CreateProcess -> P.CreateProcess
configureIsolatedProcess process =
  process
    { P.create_group = not isWindows,
      P.use_process_jobs = isWindows,
      P.std_in = P.NoStream,
      P.std_out = P.CreatePipe,
      P.std_err = P.CreatePipe
    }

killStartedProcessGroup :: Maybe P.Pid -> IO ()
#if mingw32_HOST_OS
killStartedProcessGroup _ = return ()
#else
killStartedProcessGroup Nothing = return ()
killStartedProcessGroup (Just processGroupPid) =
  void (try (signalProcessGroupIfAlive Signals.sigKILL processGroupPid) :: IO (Either SomeException ()))
#endif

stopProcessGroup :: P.ProcessHandle -> Async.Async ExitCode -> Maybe P.Pid -> IO Bool
#if mingw32_HOST_OS
stopProcessGroup processHandle rootExitAsync _ = do
  void (try (P.terminateProcess processHandle) :: IO (Either SomeException ()))
  waitForAsync rootExitAsync hardStopTimeoutMicroseconds
#else
-- Signals target the isolated group. After SIGKILL, we wait for the root,
-- because POSIX cannot wait for descendants that are no longer our children.
-- The result confirms only that the root exited.
stopProcessGroup processHandle rootExitAsync maybeProcessGroupPid =
  case maybeProcessGroupPid of
    Nothing -> stopRootProcess processHandle rootExitAsync
    Just processGroupPid -> do
      signalProcessGroupIfAlive Signals.sigINT processGroupPid
      stoppedGracefully <- waitForProcessGroupExit processGroupPid gracefulStopTimeoutMicroseconds
      unless stoppedGracefully $ signalProcessGroupIfAlive Signals.sigKILL processGroupPid
      waitForAsync rootExitAsync hardStopTimeoutMicroseconds
#endif

stopRootProcess :: P.ProcessHandle -> Async.Async ExitCode -> IO Bool
stopRootProcess processHandle rootExitAsync = do
  void (try (P.terminateProcess processHandle) :: IO (Either SomeException ()))
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
signalProcessGroupIfAlive signal processGroupPid =
  Signals.signalProcessGroup signal processGroupPid `catchIOError` \ioErr ->
    -- macOS can report EPERM when only zombies remain in the group.
    unless (isDoesNotExistError ioErr || isPermissionError ioErr) $ ioError ioErr

waitForProcessGroupExit :: P.Pid -> Int -> IO Bool
waitForProcessGroupExit processGroupPid = waitForCondition $ not <$> isProcessGroupAlive processGroupPid

isProcessGroupAlive :: P.Pid -> IO Bool
isProcessGroupAlive processGroupPid =
  (Signals.signalProcessGroup Signals.nullSignal processGroupPid >> return True)
    `catchIOError` handleProbeError
  where
    handleProbeError err
      | isDoesNotExistError err = return False
      | isPermissionError err = return True
      | otherwise = ioError err
#endif

waitForCondition :: IO Bool -> Int -> IO Bool
waitForCondition condition timeoutMicroseconds
  | timeoutMicroseconds <= 0 = return False
  | otherwise = fromMaybe False <$> timeout timeoutMicroseconds loop
  where
    loop = do
      conditionMet <- condition
      if conditionMet
        then return True
        else do
          threadDelay pollIntervalMicroseconds
          loop

gracefulStopTimeoutMicroseconds :: Int
gracefulStopTimeoutMicroseconds = secondsToMicroSeconds 0.25

hardStopTimeoutMicroseconds :: Int
hardStopTimeoutMicroseconds = secondsToMicroSeconds 2

pollIntervalMicroseconds :: Int
pollIntervalMicroseconds = secondsToMicroSeconds 0.1

isWindows :: Bool
#if mingw32_HOST_OS
isWindows = True
#else
isWindows = False
#endif
