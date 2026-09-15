module Job.SubprocessTest where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import qualified Wasp.Job as J
import qualified Wasp.Job.Subprocess as Subprocess
import Wasp.Util (secondsToMicroSeconds)

spec_runSubprocess :: Spec
spec_runSubprocess =
  describe "Subprocess.runChecked" $ do
    it "decodes split and incomplete UTF-8 on stdout" $
      runSplitUtf8Process "stdout" J.Stdout `shouldReturn` "€�"

    it "decodes split and incomplete UTF-8 on stderr" $
      runSplitUtf8Process "stderr" J.Stderr `shouldReturn` "€�"

    it "fails the Job on a nonzero child exit" $ do
      chan <- newChan
      let action = Subprocess.runChecked $ P.proc "node" ["-e", "process.exit(7)"]
      J.runJob (J.makeJob J.Wasp action) chan `shouldReturn` ExitFailure 7

    it "can return a nonzero child exit for explicit handling" $ do
      chan <- newChan
      let action = do
            exitCode <- Subprocess.runReturningExitCode $ P.proc "node" ["-e", "process.exit(7)"]
            liftIO $ exitCode `shouldBe` ExitFailure 7
      J.runJob (J.makeJob J.Wasp action) chan `shouldReturn` ExitSuccess

runSplitUtf8Process :: String -> J.JobOutputKind -> IO T.Text
runSplitUtf8Process streamName expectedOutputKind = do
  chan <- newChan
  let action = Subprocess.runChecked $ P.proc "node" ["-e", splitUtf8Script streamName]
  exitCode <- J.runJob (J.makeJob J.Wasp action) chan
  exitCode `shouldBe` ExitSuccess
  output <- collectOutputUntilExit expectedOutputKind chan
  remainingEvent <- timeout (secondsToMicroSeconds 0.1) $ readChan chan
  remainingEvent `shouldSatisfy` isNothing
  return output

splitUtf8Script :: String -> String
splitUtf8Script streamName =
  "process."
    <> streamName
    <> ".write(Buffer.from([0xe2])); setTimeout(() => process."
    <> streamName
    <> ".write(Buffer.from([0x82, 0xac, 0xe2])), 200);"

collectOutputUntilExit :: J.JobOutputKind -> Chan J.JobEvent -> IO T.Text
collectOutputUntilExit expectedOutputKind chan = go []
  where
    go collected = do
      event <- readChan chan
      J._jobKind event `shouldBe` J.Wasp
      case J._eventData event of
        J.JobOutput outputKind output -> do
          outputKind `shouldBe` expectedOutputKind
          go (output : collected)
        J.JobExited exitCode -> do
          exitCode `shouldBe` ExitSuccess
          return $ T.concat $ reverse collected
