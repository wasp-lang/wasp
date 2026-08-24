module Wasp.Cli.PortTest where

import Control.Exception (bracket)
import Data.Either (isRight)
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

spec_findFirstFreeLocalPortInRange :: Spec
spec_findFirstFreeLocalPortInRange = do
  describe "findFirstFreeLocalPortInRange" $ do
    it "returns the first port in the range when it is free" $ do
      withFreeLocalPort $ \freePort ->
        findFirstFreeLocalPortInRange freePort [] remediationHint
          `shouldReturn` Right freePort

    -- We can't assert which port exactly it returns: it returns the next free
    -- one, and we don't know which ports the machine has available.
    it "doesn't return a taken port" $ do
      withTakenLocalPort $ \takenPort -> do
        result <- findFirstFreeLocalPortInRange takenPort [] remediationHint
        result `shouldSatisfy` isRight
        result `shouldNotBe` Right takenPort

    it "doesn't return a port it was told to skip" $ do
      withFreeLocalPort $ \freePort -> do
        result <- findFirstFreeLocalPortInRange freePort [freePort] remediationHint
        result `shouldSatisfy` isRight
        result `shouldNotBe` Right freePort

    it "reports the checked range and how to fix it when no port is free" $ do
      withFreeLocalPort $ \freePort -> do
        -- We tell it to skip many more ports than it checks, so it is left with
        -- no port to check at all.
        result <- findFirstFreeLocalPortInRange freePort [freePort .. freePort + 100] remediationHint
        case result of
          Right port -> expectationFailure $ "Expected an error, but got port " ++ show port
          Left err -> do
            err `shouldContain` show freePort
            err `shouldContain` remediationHint
  where
    remediationHint = "Free up some ports."

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
