module Job.ProcessTest where

import Control.Concurrent (newChan)
import qualified Data.Text as T
import Test.Hspec
import qualified Wasp.Job as J
import Wasp.Job.IO (collectJobTextOutputUntilExitReceived)
import Wasp.Job.Process (runProcessAsJob)
import System.Exit (ExitCode (ExitSuccess))
import qualified System.Process as P

spec_runProcessAsJob :: Spec
spec_runProcessAsJob =
  describe "runProcessAsJob" $ do
    it "does not crash when stdout contains UTF-8 chars split across chunks" $ do
      let textToPrint = T.replicate 200000 "€"
      output <- runNodeScriptAndCollectTextOutput "process.stdout.write('€'.repeat(200000));"
      output `shouldBe` textToPrint

    it "does not crash when stderr contains UTF-8 chars split across chunks" $ do
      let textToPrint = T.replicate 200000 "€"
      output <- runNodeScriptAndCollectTextOutput "process.stderr.write('€'.repeat(200000));"
      output `shouldBe` textToPrint

runNodeScriptAndCollectTextOutput :: String -> IO T.Text
runNodeScriptAndCollectTextOutput script = do
  chan <- newChan
  exitCode <- runProcessAsJob (P.proc "node" ["-e", script]) J.Wasp chan
  exitCode `shouldBe` ExitSuccess
  reverseChunks <- collectJobTextOutputUntilExitReceived chan
  return $ T.concat $ reverse reverseChunks
