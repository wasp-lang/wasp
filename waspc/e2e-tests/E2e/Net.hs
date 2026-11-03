{-# LANGUAGE TypeApplications #-}

module E2e.Net
  ( waitUntilListening,
    waitUntilNotListening,
  )
where

import Control.Exception (IOException, bracket, try)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import E2e.TestM (TestM, waitUntil)
import qualified Network.Socket as Socket

-- | Waits until something listens on all of the ports.
waitUntilListening :: [Int] -> Int -> TestM ()
waitUntilListening ports seconds =
  waitUntil ("something listens on ports " ++ show ports) seconds
    $ liftIO
    $ and <$> mapM isListening ports

-- | Waits until nothing listens on any of the ports anymore.
waitUntilNotListening :: [Int] -> Int -> TestM ()
waitUntilNotListening ports seconds =
  waitUntil ("nothing listens on ports " ++ show ports) seconds
    $ liftIO
    $ not . or <$> mapM isListening ports

-- | Connects to the port, without sending anything, so that it can't hang on
-- an app that accepts connections but doesn't respond.
--
-- We try all addresses of @localhost@, since e.g. Vite listens only on one of
-- IPv4 and IPv6.
isListening :: Int -> IO Bool
isListening port = do
  addresses <-
    fromRight []
      <$> (try @IOException $ Socket.getAddrInfo (Just hints) (Just "localhost") (Just $ show port))
  or <$> mapM canConnect addresses
  where
    hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}

    canConnect address =
      either (const False) (const True)
        <$> try @IOException
          ( bracket
              (Socket.socket (Socket.addrFamily address) Socket.Stream Socket.defaultProtocol)
              Socket.close
              (`Socket.connect` Socket.addrAddress address)
          )
