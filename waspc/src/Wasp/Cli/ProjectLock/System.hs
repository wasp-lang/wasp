{-# LANGUAGE CPP #-}

module Wasp.Cli.ProjectLock.System
  ( WaspProcessId,
    getCurrentWaspProcessId,
    isProcessAlive,
  )
where

import Control.Exception (IOException, displayException, try)
import System.IO.Error (isDoesNotExistError)

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process as Process
#else
import qualified System.Posix.Process as Process
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as Posix
#endif

type WaspProcessId = Integer

getCurrentWaspProcessId :: IO WaspProcessId
#ifdef mingw32_HOST_OS
getCurrentWaspProcessId =
  fromIntegral <$> Process.getCurrentProcessId
#else
getCurrentWaspProcessId =
  fromIntegral <$> Process.getProcessID
#endif

isProcessAlive :: WaspProcessId -> IO (Either String Bool)
#ifdef mingw32_HOST_OS
isProcessAlive processId = do
  processesOrError <-
    try $
      Process.withTh32Snap Process.tH32CS_SNAPPROCESS Nothing Process.th32SnapEnumProcesses
  return $ case processesOrError of
    Left ioException -> Left $ displayException (ioException :: IOException)
    Right processes ->
      Right $
        any
          (\(candidateProcessId, _, _, _, _) -> fromIntegral candidateProcessId == processId)
          processes
#else
isProcessAlive processId = do
  signalResult <-
    try $
      Signals.signalProcess Signals.nullSignal (fromIntegral processId :: Posix.ProcessID)
  return $ case signalResult of
    Right () -> Right True
    Left ioException
      | isDoesNotExistError ioException -> Right False
      | otherwise -> Left $ displayException (ioException :: IOException)
#endif
