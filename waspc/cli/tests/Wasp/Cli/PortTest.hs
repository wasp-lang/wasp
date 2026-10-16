module Wasp.Cli.PortTest where

import Control.Exception (bracket)
import qualified Network.Socket as S
import Test.Hspec
import Wasp.Cli.Port (findFirstFreeLocalPortAmong, findFirstFreeLocalPortInRange)
import qualified Wasp.Util.Network.Socket as Socket

-- We test with fixed ports below the range the OS hands out ports from (it
-- starts at 32768), assuming they are free, and give each test its own ports so
-- that tests running in parallel don't interfere with each other.
--
-- We can't ask the OS for a port instead: 'findFirstFreeLocalPortInRange' scans
-- a range of consecutive ports, and some OSs (e.g., Windows) hand out ports
-- for outgoing connections from its own range in the same order, so the scan
-- ends up
-- connecting to the very port it is connecting from, which makes free ports
-- look taken.

spec_findFirstFreeLocalPortAmong :: Spec
spec_findFirstFreeLocalPortAmong = do
  describe "findFirstFreeLocalPortAmong" $ do
    it "returns Nothing when there are no candidate ports" $ do
      findFirstFreeLocalPortAmong [] `shouldReturn` Nothing

    it "returns the first candidate port when it is free" $ do
      findFirstFreeLocalPortAmong [20400] `shouldReturn` Just 20400

    it "skips a taken port and returns the next free one" $ do
      withTakenLocalPortsAt [20500] $
        findFirstFreeLocalPortAmong [20500, 20501] `shouldReturn` Just 20501

    it "returns Nothing when all candidate ports are taken" $ do
      withTakenLocalPortsAt [20600, 20601] $
        findFirstFreeLocalPortAmong [20600, 20601] `shouldReturn` Nothing

spec_findFirstFreeLocalPortInRange :: Spec
spec_findFirstFreeLocalPortInRange = do
  describe "findFirstFreeLocalPortInRange" $ do
    it "returns the first port in the range when it is free" $ do
      findFirstFreeLocalPortInRange 20000 [] remediationHint
        `shouldReturn` Right 20000

    it "returns the next port when the first one is taken" $ do
      withTakenLocalPortsAt [20100] $
        findFirstFreeLocalPortInRange 20100 [] remediationHint
          `shouldReturn` Right 20101

    it "returns the next port when the first one is skipped" $ do
      findFirstFreeLocalPortInRange 20200 [20200] remediationHint
        `shouldReturn` Right 20201

    it "reports the checked range and how to fix it when no port is free" $ do
      -- We take more ports than it checks, so it can't find a free one.
      withTakenLocalPortsAt [20300 .. 20350] $ do
        result <- findFirstFreeLocalPortInRange 20300 [] remediationHint
        case result of
          Right port -> expectationFailure $ "Expected an error, but got port " ++ show port
          Left err -> do
            err `shouldContain` "20300"
            err `shouldContain` remediationHint

    it "reports that it had nothing to check when all ports are skipped" $ do
      -- We tell it to skip more ports than it checks, so it is left with no port
      -- to check at all.
      result <- findFirstFreeLocalPortInRange 20700 [20700 .. 20750] remediationHint
      case result of
        Right port -> expectationFailure $ "Expected an error, but got port " ++ show port
        Left err -> do
          err `shouldContain` "20700"
          err `shouldContain` "skipped"
  where
    remediationHint = "Free up some ports."

withTakenLocalPortsAt :: [S.PortNumber] -> IO a -> IO a
withTakenLocalPortsAt ports action =
  bracket (mapM openLocalSocketOnPort ports) (mapM_ S.close) (const action)

openLocalSocketOnPort :: S.PortNumber -> IO S.Socket
openLocalSocketOnPort port = do
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.bind sock $ Socket.makeLocalHostSocketAddress port
  S.listen sock 1
  return sock
