module Job.ProcessTest where

import Control.Concurrent (Chan, newChan, readChan)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import qualified Wasp.Job as J
import qualified Wasp.Job.Process as Process
import Wasp.Util (secondsToMicroSeconds)

spec_runProcessAsJob :: Spec
spec_runProcessAsJob =
  describe "runProcessAsJob" $ do
    it "decodes split and incomplete UTF-8 on stdout" $
      runSplitUtf8Process "stdout" J.Stdout `shouldReturn` "€�"

    it "decodes split and incomplete UTF-8 on stderr" $
      runSplitUtf8Process "stderr" J.Stderr `shouldReturn` "€�"

runSplitUtf8Process :: String -> J.JobOutputType -> IO T.Text
runSplitUtf8Process streamName expectedOutputType = do
  chan <- newChan
  exitCode <- Process.runProcessAsJob (P.proc "node" ["-e", splitUtf8Script streamName]) J.Wasp chan
  exitCode `shouldBe` ExitSuccess
  output <- collectOutputUntilExit expectedOutputType chan
  remainingMessage <- timeout (secondsToMicroSeconds 0.1) $ readChan chan
  remainingMessage `shouldSatisfy` isNothing
  return output

splitUtf8Script :: String -> String
splitUtf8Script streamName =
  "process."
    <> streamName
    <> ".write(Buffer.from([0xe2])); setTimeout(() => process."
    <> streamName
    <> ".write(Buffer.from([0x82, 0xac, 0xe2])), 200);"

collectOutputUntilExit :: J.JobOutputType -> Chan J.JobMessage -> IO T.Text
collectOutputUntilExit expectedOutputType chan = go []
  where
    go collected = do
      message <- readChan chan
      J._jobType message `shouldBe` J.Wasp
      case J._data message of
        J.JobOutput output outputType -> do
          outputType `shouldBe` expectedOutputType
          go (output : collected)
        J.JobExit exitCode -> do
          exitCode `shouldBe` ExitSuccess
          return $ T.concat $ reverse collected
