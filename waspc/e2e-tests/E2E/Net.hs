{-# LANGUAGE TypeApplications #-}

-- | Waiting for processes to open and close their ports.
module E2E.Net
  ( waitUntilListening,
    waitUntilStopped,
    interruptProcessGroupAndWaitUntilStopped,
  )
where

import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import E2E.Process (RunningProcess, failIfExited, failWithOutput, signalProcessGroupOf, waitForExit)
import E2E.Signals (Signal (..))
import E2E.TestM (TestM, failTest, pollUntil, waitUntil)
import Foreign.C.Error (Errno (..), eCONNREFUSED, eCONNRESET)
import GHC.IO.Exception (IOException (ioe_errno))
import qualified Network.Socket as Socket
import System.Exit (ExitCode)

-- | Waits until something listens on all of the ports. Fails the test with the
-- process's output if it exits first or the ports don't open in time.
waitUntilListening :: RunningProcess -> [Int] -> Int -> TestM ()
waitUntilListening runningProcess ports seconds = do
  isEverythingListening <- pollUntil seconds $ do
    failIfExited runningProcess ("something listens on ports " ++ show ports)
    and <$> mapM isListening ports
  unless isEverythingListening $
    failWithOutput runningProcess ("didn't open ports " ++ show ports ++ " within " ++ show seconds ++ "s")

-- | Waits until nothing listens on any of the ports anymore.
waitUntilNotListening :: [Int] -> Int -> TestM ()
waitUntilNotListening ports seconds =
  waitUntil ("nothing listens on ports " ++ show ports) seconds $
    not . or <$> mapM isListening ports

-- | Waits for a process we told to stop (e.g. with a signal) to exit, and then
-- until nothing listens on its ports anymore. With no ports, it only waits for
-- the process to exit.
waitUntilStopped :: RunningProcess -> [Int] -> TestM ExitCode
waitUntilStopped runningProcess ports = do
  exitCode <- waitForExit runningProcess stopTimeoutSeconds
  waitUntilNotListening ports portsFreeTimeoutSeconds
  return exitCode
  where
    stopTimeoutSeconds = 60
    -- Children that are still stopping can keep the ports open a bit longer.
    portsFreeTimeoutSeconds = 30

-- | Stops the process like Ctrl+C in a terminal does, by interrupting its
-- whole process group, and waits until it's stopped (see 'waitUntilStopped').
--
-- For commands that don't take the project lock. The tests of `wasp start`
-- read Wasp's PID from the lock to signal only Wasp, but these commands don't
-- write it. So this only checks that interrupting everything stops everything,
-- and not that Wasp stops the processes it started.
interruptProcessGroupAndWaitUntilStopped :: RunningProcess -> [Int] -> TestM ExitCode
interruptProcessGroupAndWaitUntilStopped runningProcess ports = do
  signalProcessGroupOf SIGINT runningProcess
  waitUntilStopped runningProcess ports

-- | Connects to the port, without sending anything, so that it can't hang on
-- an app that accepts connections but doesn't respond.
--
-- We try all addresses of @localhost@, since e.g. Vite listens only on one of
-- IPv4 and IPv6.
--
-- Only a refused or reset connection means that nothing listens: the OS
-- refuses connections to a local port nothing listens on right away, and on
-- both macOS and Linux, it resets them instead when the listener closes while
-- we connect. Other errors (e.g. EADDRNOTAVAIL when we run out of local ports,
-- or a timeout) say nothing about the port, so they fail the test, since
-- treating them as "not listening" would let 'waitUntilNotListening' pass
-- without checking anything.
isListening :: Int -> TestM Bool
isListening port = do
  addresses <- liftIO $ Socket.getAddrInfo (Just hints) (Just "localhost") (Just $ show port)
  or <$> mapM canConnect addresses
  where
    hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}

    canConnect address = do
      connectResult <-
        liftIO . try @IOException $
          bracket
            (Socket.socket (Socket.addrFamily address) Socket.Stream Socket.defaultProtocol)
            Socket.close
            (`Socket.connect` Socket.addrAddress address)
      case connectResult of
        Right () -> return True
        Left err
          | isRefusedOrReset err -> return False
          | otherwise ->
              failTest $ "Couldn't check whether something listens on " ++ show (Socket.addrAddress address) ++ ": " ++ displayException err

    isRefusedOrReset err = (Errno <$> ioe_errno err) `elem` map Just [eCONNREFUSED, eCONNRESET]
