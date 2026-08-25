module Wasp.Cli.PortTest where

import Control.Exception (bracket)
import qualified Network.Socket as S
import Test.Hspec
import Wasp.Cli.Port (findFirstFreeLocalPortAmong, findFirstFreeLocalPortInRange)
import qualified Wasp.Util.Network.Socket as Socket

spec_findFirstFreeLocalPortAmong :: Spec
spec_findFirstFreeLocalPortAmong = do
  describe "findFirstFreeLocalPortAmong" $ do
    it "returns Nothing when there are no candidate ports" $ do
      findFirstFreeLocalPortAmong [] `shouldReturn` Nothing

    it "returns the first candidate port when it is free" $ do
      withFreeLocalPort $ \freePort ->
        findFirstFreeLocalPortAmong [freePort] `shouldReturn` Just freePort

    it "skips a taken port and returns the next free one" $ do
      withTakenLocalPort $ \takenPort ->
        withFreeLocalPort $ \freePort ->
          findFirstFreeLocalPortAmong [takenPort, freePort] `shouldReturn` Just freePort

    it "returns Nothing when all candidate ports are taken" $ do
      withTakenLocalPort $ \takenPort1 ->
        withTakenLocalPort $ \takenPort2 ->
          findFirstFreeLocalPortAmong [takenPort1, takenPort2] `shouldReturn` Nothing

-- | These tests scan ranges of consecutive ports, so they can't start from a
-- port the OS gave them: the OS hands out ports for outgoing connections from
-- the same range and in the same order, so the scan ends up connecting to the
-- very port it is connecting from, which makes free ports look taken.
-- We therefore use ports below the range the OS hands out from (it starts at
-- 32768), assuming they are free, and give each test its own range so that
-- tests running in parallel don't scan into each other's ports.
spec_findFirstFreeLocalPortInRange :: Spec
spec_findFirstFreeLocalPortInRange = do
  describe "findFirstFreeLocalPortInRange" $ do
    it "returns the first port in the range when it is free" $ do
      findFirstFreeLocalPortInRange 20000 [] remediationHint
        `shouldReturn` Right 20000

    it "returns the next port when the first one is taken" $ do
      withTakenLocalPortAt 20100 $
        findFirstFreeLocalPortInRange 20100 [] remediationHint
          `shouldReturn` Right 20101

    it "returns the next port when the first one is skipped" $ do
      findFirstFreeLocalPortInRange 20200 [20200] remediationHint
        `shouldReturn` Right 20201

    it "reports the checked range and how to fix it when no port is free" $ do
      -- We tell it to skip many more ports than it checks, so it is left with
      -- no port to check at all.
      result <- findFirstFreeLocalPortInRange 20300 [20300 .. 20400] remediationHint
      case result of
        Right port -> expectationFailure $ "Expected an error, but got port " ++ show port
        Left err -> do
          err `shouldContain` "20300"
          err `shouldContain` remediationHint
  where
    remediationHint = "Free up some ports."

withFreeLocalPort :: (S.PortNumber -> IO a) -> IO a
withFreeLocalPort action =
  bracket openLocalSocketOnAnyFreePort (S.close . fst) (return . snd) >>= action

withTakenLocalPort :: (S.PortNumber -> IO a) -> IO a
withTakenLocalPort action =
  bracket openLocalSocketOnAnyFreePort (S.close . fst) (action . snd)

withTakenLocalPortAt :: S.PortNumber -> IO a -> IO a
withTakenLocalPortAt port action =
  bracket (openLocalSocketOnPort port) S.close (const action)

openLocalSocketOnAnyFreePort :: IO (S.Socket, S.PortNumber)
openLocalSocketOnAnyFreePort = do
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.bind sock $ Socket.makeLocalHostSocketAddress S.defaultPort
  S.listen sock 1
  port <- S.socketPort sock
  return (sock, port)

openLocalSocketOnPort :: S.PortNumber -> IO S.Socket
openLocalSocketOnPort port = do
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.bind sock $ Socket.makeLocalHostSocketAddress port
  S.listen sock 1
  return sock
