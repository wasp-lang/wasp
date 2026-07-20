module Job.ProcessTest where

import Control.Concurrent (Chan, newChan, readChan)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import qualified Wasp.Job as J
import qualified Wasp.Job.Process as Process
import Wasp.Util (secondsToMicroSeconds)

spec_runProcessAndStreamOutput :: Spec
spec_runProcessAndStreamOutput =
  describe "runProcessAndStreamOutput" $ do
    it "decodes split and incomplete UTF-8 on stdout" $
      runSplitUtf8Process "stdout" `shouldReturn` "€�"

    it "decodes split and incomplete UTF-8 on stderr" $
      runSplitUtf8Process "stderr" `shouldReturn` "€�"

runSplitUtf8Process :: String -> IO T.Text
runSplitUtf8Process streamName = do
  chan <- newChan
  exitCode <- Process.runProcessAndStreamOutput (P.proc "node" ["-e", splitUtf8Script streamName]) J.Wasp chan
  exitCode `shouldBe` ExitSuccess
  collectQueuedOutput chan

splitUtf8Script :: String -> String
splitUtf8Script streamName =
  "process."
    <> streamName
    <> ".write(Buffer.from([0xe2])); setTimeout(() => process."
    <> streamName
    <> ".write(Buffer.from([0x82, 0xac, 0xe2])), 200);"

collectQueuedOutput :: Chan J.JobMessage -> IO T.Text
collectQueuedOutput chan = go []
  where
    go collected = do
      maybeMessage <- timeout (secondsToMicroSeconds 0.2) $ readChan chan
      case maybeMessage of
        Nothing -> return $ T.concat $ reverse collected
        Just J.JobMessage {J._data = J.JobOutput output _} -> go (output : collected)
        Just _ -> go collected
