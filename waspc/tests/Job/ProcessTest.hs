module Job.ProcessTest where

import Control.Concurrent (newChan)
import qualified Data.Conduit.Process as CP
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec
import qualified Wasp.Job as J
import Wasp.Job.IO (collectJobTextOutputUntilExitReceived)
import Wasp.Job.Process (runProcessAsJob)

spec_runProcessAsJob :: Spec
spec_runProcessAsJob =
  describe "runProcessAsJob" $ do
    it "handles UTF-8 chars split across stdout chunks" $ do
      output <- runNodeScriptAndCollectTextOutput "stdout"
      output `shouldBe` T.replicate 10 "€"

    it "handles UTF-8 chars split across stderr chunks" $ do
      output <- runNodeScriptAndCollectTextOutput "stderr"
      output `shouldBe` T.replicate 10 "€"

runNodeScriptAndCollectTextOutput :: String -> IO T.Text
runNodeScriptAndCollectTextOutput outputStreamName = do
  chan <- newChan
  exitCode <- runProcessAsJob (CP.proc "node" ["tests/Job/splitUtf8Chars.js", outputStreamName]) J.Wasp chan
  exitCode `shouldBe` ExitSuccess
  reverseChunks <- collectJobTextOutputUntilExitReceived chan
  return $ T.concat $ reverse reverseChunks
