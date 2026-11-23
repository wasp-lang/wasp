{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-- | Sending and handling signals, in IO. Test steps that send them are in
-- "E2E.Process".
--
-- The only module with code that differs by platform. We don't run the tests
-- on Windows (see @Main.hs@), so there its functions fail or do nothing.
module E2E.Signals
  ( Signal (..),
    signalProcess,
    signalProcessGroup,
    processGroupExists,
    stopOnFirstCtrlC,
  )
where

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt), catch, throwIO)
import Control.Monad (void)
import Data.IORef (atomicModifyIORef', newIORef)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Signals as Posix
#endif

data Signal = SIGINT | SIGTERM | SIGHUP | SIGKILL
  deriving (Show)

signalProcess :: Signal -> Int -> IO ()

-- | The process group ID is the PID of the process that created the group.
signalProcessGroup :: Signal -> Int -> IO ()
processGroupExists :: Int -> IO Bool

-- | Makes the first Ctrl+C interrupt the main thread, like it does by default,
-- so tasty stops the tests and their cleanups stop the processes they started.
-- After it, we ignore further Ctrl+Cs and SIGTERMs, so that nothing cuts the
-- cleanups short: by default, GHC exits on the second Ctrl+C, and tasty on
-- SIGTERM, which e.g. mise sends on the second Ctrl+C.
stopOnFirstCtrlC :: IO ()

#if defined(mingw32_HOST_OS)
processGroupExists _ = fail "Process groups are not supported on Windows."
signalProcess sig _ = fail $ "Sending " ++ show sig ++ " is not supported on Windows."
signalProcessGroup sig _ = fail $ "Sending " ++ show sig ++ " is not supported on Windows."
stopOnFirstCtrlC = return ()
#else
signalProcess sig pid = Posix.signalProcess (toPosixSignal sig) (fromIntegral pid)
signalProcessGroup sig pid = Posix.signalProcessGroup (toPosixSignal sig) (fromIntegral pid)

-- | Sending the null signal only checks whether the group has any processes.
-- It fails with ESRCH (a does-not-exist error) when there are none. Any other
-- error (e.g. EPERM, when the group belongs to another user) means we can't
-- tell, so we rethrow it.
processGroupExists pgid =
  (Posix.signalProcessGroup Posix.nullSignal (fromIntegral pgid) >> return True)
    `catch` \err -> if isDoesNotExistError err then return False else throwIO err

stopOnFirstCtrlC = do
  mainThread <- myThreadId
  interruptedRef <- newIORef False
  installHandler Posix.sigINT $ do
    wasInterrupted <- atomicModifyIORef' interruptedRef (True,)
    if wasInterrupted
      then stillStopping
      else do
        -- Replaces tasty's handler, which it installs after this one.
        installHandler Posix.sigTERM (return ())
        throwTo mainThread UserInterrupt
  where
    installHandler sig handler = void $ Posix.installHandler sig (Posix.Catch handler) Nothing
    stillStopping = hPutStrLn stderr "Still stopping the processes the tests started..."

toPosixSignal :: Signal -> Posix.Signal
toPosixSignal = \case
  SIGINT -> Posix.sigINT
  SIGTERM -> Posix.sigTERM
  SIGHUP -> Posix.sigHUP
  SIGKILL -> Posix.sigKILL
#endif
