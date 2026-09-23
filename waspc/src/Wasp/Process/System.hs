{-# LANGUAGE CPP #-}

module Wasp.Process.System
  ( configureIsolatedProcess,
    hardStopTimeoutMicroseconds,
    stopProcessGroup,
  )
where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, void)
import Control.Monad.Extra (anyM)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode)
import qualified System.Process as P
import System.Timeout (timeout)
import Wasp.Util (secondsToMicroSeconds)

#if !mingw32_HOST_OS
import qualified Data.ByteString.Char8 as BSC
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError, isPermissionError, tryIOError)
import qualified System.Posix.Signals as Signals
import Text.Read (readMaybe)
#endif

configureIsolatedProcess :: P.CreateProcess -> P.CreateProcess
configureIsolatedProcess process =
  process
    { P.create_group = not isWindows,
      P.use_process_jobs = isWindows,
      P.std_in = P.NoStream,
      P.std_out = P.CreatePipe,
      P.std_err = P.CreatePipe
    }

stopProcessGroup :: P.ProcessHandle -> Async.Async ExitCode -> Maybe P.Pid -> IO Bool
#if mingw32_HOST_OS
stopProcessGroup processHandle rootExitAsync _ = do
  void (try (P.terminateProcess processHandle) :: IO (Either SomeException ()))
  waitForAsync rootExitAsync hardStopTimeoutMicroseconds
#else
stopProcessGroup processHandle rootExitAsync maybeProcessGroupPid =
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
    -- On macOS, EPERM can also mean a zombie-only group. The bounded wait
    -- must still confirm that the group has stopped.
    unless (isDoesNotExistError ioErr || isPermissionError ioErr) $ ioError ioErr

waitForProcessGroupExit :: P.Pid -> Int -> IO Bool
waitForProcessGroupExit processGroupPid = waitForCondition $ not <$> isProcessGroupAlive processGroupPid

isProcessGroupAlive :: P.Pid -> IO Bool
isProcessGroupAlive processGroupPid = do
  processGroupExists <-
    (Signals.signalProcessGroup Signals.nullSignal processGroupPid >> return True)
      `catchIOError` handleProbeError
  if not processGroupExists || not isLinux
    then return processGroupExists
    else do
      hasLiveMember <- linuxProcessGroupMayHaveLiveMember processGroupPid
      if hasLiveMember
        then return True
        else do
          -- A group member can fork while the first /proc snapshot is being
          -- read. Only call a zombie-only group quiescent after two scans.
          threadDelay linuxZombieConfirmationMicroseconds
          linuxProcessGroupMayHaveLiveMember processGroupPid
  where
    handleProbeError err
      | isDoesNotExistError err = return False
      | isPermissionError err = return True
      | otherwise = ioError err

linuxProcessGroupMayHaveLiveMember :: P.Pid -> IO Bool
linuxProcessGroupMayHaveLiveMember processGroupPid = do
  procEntriesResult <- tryIOError $ listDirectory "/proc"
  case procEntriesResult of
    Left _ -> return True
    Right procEntries -> anyM isLiveGroupMember procEntries
  where
    processGroupId = show processGroupPid

    isLiveGroupMember procEntry =
      case readMaybe procEntry :: Maybe Int of
        Nothing -> return False
        Just _ -> do
          processStateAndGroup <-
            tryIOError $ parseLinuxProcStat <$> BS.readFile ("/proc" </> procEntry </> "stat")
          return $ case processStateAndGroup of
            Right (Just (processState, memberProcessGroupId)) ->
              memberProcessGroupId == processGroupId
                && processState /= "Z"
                && processState /= "X"
            Left ioErr -> not $ isDoesNotExistError ioErr
            Right Nothing -> True

parseLinuxProcStat :: BS.ByteString -> Maybe (String, String)
parseLinuxProcStat procStat =
  case break (== ')') $ reverse $ BSC.unpack procStat of
    (_, []) -> Nothing
    (reversedFieldsAfterCommand, _ : _) ->
      case words $ reverse reversedFieldsAfterCommand of
        processState : _parentPid : processGroupId : _ -> Just (processState, processGroupId)
        _ -> Nothing

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
