module Wasp.Util.Network.LocalAddress
  ( getLocalNetworkIPv4,
    toNipIoHostname,
    isIPv4Address,
  )
where

import Control.Exception (SomeException, try)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Network.Socket as S
import UnliftIO.Exception (bracket)

-- | Returns the local IPv4 address the operating system would use to reach an
-- external host, formatted as a dotted-decimal string. This is usually the LAN
-- address of the machine on its primary interface.
--
-- The implementation uses a UDP socket trick that does not send any packets:
-- creating and connecting a datagram socket only sets the kernel's routing
-- entry, after which we read back the socket's local address.
--
-- Returns Nothing when no usable IPv4 address can be determined (e.g. when the
-- machine has no network interfaces or DNS lookup fails).
getLocalNetworkIPv4 :: IO (Maybe String)
getLocalNetworkIPv4 = do
  result <- try $ bracket createUdpSocket S.close $ \sock -> do
    S.connect sock arbitraryPublicAddress
    sockName <- S.getSocketName sock
    return $ case sockName of
      S.SockAddrInet _ hostAddr ->
        let (a, b, c, d) = S.hostAddressToTuple hostAddr
         in Just $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
      _ -> Nothing
  case (result :: Either SomeException (Maybe String)) of
    Left _ -> return Nothing
    Right mIp -> return mIp
  where
    createUdpSocket = S.socket S.AF_INET S.Datagram S.defaultProtocol
    -- Cloudflare's public DNS resolver. Any routable public IPv4 works here;
    -- nothing is actually sent because we never write to the socket.
    arbitraryPublicAddress = S.SockAddrInet 80 (S.tupleToHostAddress (1, 1, 1, 1))

-- | If the input parses as an IPv4 dotted-decimal address, returns the
-- equivalent @<ip>.nip.io@ hostname. Otherwise returns the input unchanged.
--
-- nip.io is a public wildcard DNS service that resolves @a.b.c.d.nip.io@ back
-- to @a.b.c.d@, giving us a real hostname for an IP. This matters because
-- several integrations (OAuth providers, cookie policies, Vite's allowedHosts)
-- treat hostnames and raw IPs differently.
toNipIoHostname :: String -> String
toNipIoHostname host
  | isIPv4Address host = host ++ ".nip.io"
  | otherwise = host

isIPv4Address :: String -> Bool
isIPv4Address s = case splitOn "." s of
  [a, b, c, d] -> all isOctet [a, b, c, d]
  _ -> False
  where
    isOctet octet =
      not (null octet)
        && length octet <= 3
        && all isDigit octet
        && (read octet :: Int) <= 255
