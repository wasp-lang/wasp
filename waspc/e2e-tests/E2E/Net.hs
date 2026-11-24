{-# LANGUAGE TypeApplications #-}

-- | Waiting for processes to open and close their ports.
module E2E.Net
  ( waitUntilListening,
    appStartTimeoutSeconds,
    waitUntilStopped,
    waitUntilNotListening,
    isListening,
  )
where

import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import E2E.Process (RunningProcess, failIfExited, failWithOutput, waitUntilExited)
import E2E.TestM (TestM, failTest, pollUntil, settleTimeoutSeconds, waitUntil)
import Foreign.C.Error (Errno (..), eADDRNOTAVAIL, eAFNOSUPPORT, eCONNREFUSED, eCONNRESET, eNETUNREACH)
import GHC.IO.Exception (IOException (ioe_errno))
import qualified Network.Socket as Socket

-- | Waits until something listens on all of the ports. Fails the test with the
-- process's output if it exits first or the ports don't open in time.
waitUntilListening :: RunningProcess -> [Int] -> Int -> TestM ()
waitUntilListening runningProcess ports seconds = do
  isEverythingListening <- pollUntil seconds $ do
    failIfExited runningProcess ("something listens on ports " ++ show ports)
    and <$> mapM isListening ports
  unless isEverythingListening $
    failWithOutput runningProcess ("didn't open ports " ++ show ports ++ " within " ++ show seconds ++ "s")

-- | How long a Wasp app may take to listen on its ports, `npm install`
-- included.
appStartTimeoutSeconds :: Int
appStartTimeoutSeconds = 5 * 60

-- | Waits for a process we told to stop (e.g. with a signal) to exit (see
-- 'waitUntilExited'), and then until nothing listens on its ports anymore.
waitUntilStopped :: RunningProcess -> [Int] -> TestM ()
waitUntilStopped runningProcess ports = do
  waitUntilExited runningProcess
  waitUntilNotListening ports

-- | Waits until nothing listens on any of the ports anymore, after the process
-- that opened them exited. Children that are still stopping can keep the
-- ports open a bit longer.
waitUntilNotListening :: [Int] -> TestM ()
waitUntilNotListening ports =
  waitUntil ("nothing listens on ports " ++ show ports) settleTimeoutSeconds $
    not . or <$> mapM isListening ports

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
--
-- The exception is an IPv6 address on a machine with IPv6 disabled, where
-- @localhost@ can still resolve to @::1@ (e.g. from @/etc/hosts@) but nothing
-- can listen on it: connecting fails with EADDRNOTAVAIL (or ENETUNREACH) on
-- Linux, and creating the socket can fail with EAFNOSUPPORT. Nothing listens
-- there, and the IPv4 address is still checked.
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
          | isIPv6 address && isIPv6Unavailable err -> return False
          | otherwise ->
              failTest $ "Couldn't check whether something listens on " ++ show (Socket.addrAddress address) ++ ": " ++ displayException err

    isRefusedOrReset = hasErrno [eCONNREFUSED, eCONNRESET]
    isIPv6Unavailable = hasErrno [eADDRNOTAVAIL, eAFNOSUPPORT, eNETUNREACH]
    hasErrno errnos err = (Errno <$> ioe_errno err) `elem` map Just errnos

    isIPv6 address = Socket.addrFamily address == Socket.AF_INET6
