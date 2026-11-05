{-# LANGUAGE CPP #-}

-- | Sending signals to processes. The only module with platform specific code.
module E2E.Signals
  ( Signal (..),
    signal,
    signalProcess,
    signalProcessGroup,
    processGroupExists,
  )
where

import Control.Monad.IO.Class (liftIO)
import E2E.TestM (TestM, logLine)
#if !defined(mingw32_HOST_OS)
import Control.Exception (IOException, catch)
import qualified System.Posix.Signals as Posix
#endif

data Signal = SIGINT | SIGTERM | SIGHUP | SIGKILL
  deriving (Show)

-- | Sends the signal to the process with the PID.
signal :: Signal -> Int -> TestM ()
signal sig pid = do
  logLine $ "\n==> Sending " ++ show sig ++ " to PID " ++ show pid
  liftIO $ signalProcess sig pid

signalProcess :: Signal -> Int -> IO ()

-- | The process group ID is the PID of the process that created the group.
signalProcessGroup :: Signal -> Int -> IO ()
processGroupExists :: Int -> IO Bool

#if defined(mingw32_HOST_OS)
processGroupExists _ = fail "Process groups are not supported on Windows."
signalProcess sig _ = fail $ "Sending " ++ show sig ++ " is not supported on Windows."
signalProcessGroup sig _ = fail $ "Sending " ++ show sig ++ " is not supported on Windows."
#else
signalProcess sig pid = Posix.signalProcess (toPosixSignal sig) (fromIntegral pid)
signalProcessGroup sig pid = Posix.signalProcessGroup (toPosixSignal sig) (fromIntegral pid)

-- | Sending the null signal only checks whether the group has any processes.
processGroupExists pgid =
  (Posix.signalProcessGroup Posix.nullSignal (fromIntegral pgid) >> return True)
    `catch` \(_ :: IOException) -> return False

toPosixSignal :: Signal -> Posix.Signal
toPosixSignal = \case
  SIGINT -> Posix.sigINT
  SIGTERM -> Posix.sigTERM
  SIGHUP -> Posix.sigHUP
  SIGKILL -> Posix.sigKILL
#endif
