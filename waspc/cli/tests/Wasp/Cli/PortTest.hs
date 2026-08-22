module Wasp.Cli.PortTest where

import Control.Exception (bracket)
import qualified Network.Socket as S
import Test.Hspec
import Wasp.Cli.Port (findFirstFreeLocalPort)
import qualified Wasp.Util.Network.Socket as Socket

spec_findFirstFreeLocalPort :: Spec
spec_findFirstFreeLocalPort = do
  describe "findFirstFreeLocalPort" $ do
    it "returns Nothing when there are no candidate ports" $ do
      findFirstFreeLocalPort [] `shouldReturn` Nothing

    it "returns the first candidate port when it is free" $ do
      withFreeLocalPort $ \freePort ->
        findFirstFreeLocalPort [freePort] `shouldReturn` Just freePort

    it "skips a taken port and returns the next free one" $ do
      withTakenLocalPort $ \takenPort ->
        withFreeLocalPort $ \freePort ->
          findFirstFreeLocalPort [takenPort, freePort] `shouldReturn` Just freePort

    it "returns Nothing when all candidate ports are taken" $ do
      withTakenLocalPort $ \takenPort1 ->
        withTakenLocalPort $ \takenPort2 ->
          findFirstFreeLocalPort [takenPort1, takenPort2] `shouldReturn` Nothing

withFreeLocalPort :: (S.PortNumber -> IO a) -> IO a
withFreeLocalPort action =
  bracket openLocalSocketOnAnyFreePort (S.close . fst) (return . snd) >>= action

withTakenLocalPort :: (S.PortNumber -> IO a) -> IO a
withTakenLocalPort action =
  bracket openLocalSocketOnAnyFreePort (S.close . fst) (action . snd)

openLocalSocketOnAnyFreePort :: IO (S.Socket, S.PortNumber)
openLocalSocketOnAnyFreePort = do
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.bind sock $ Socket.makeLocalHostSocketAddress S.defaultPort
  S.listen sock 1
  port <- S.socketPort sock
  return (sock, port)
