module Job.ProcessTest where

import Control.Concurrent (newChan)
import qualified Data.Text as T
import System.Exit (ExitCode (ExitSuccess))
import qualified System.Process as P
import Test.Hspec
import qualified Wasp.Job as J
import Wasp.Job.IO (collectJobTextOutputUntilExitReceived)
import Wasp.Job.Process (runProcessAsJob)

spec_runProcessAsJob :: Spec
spec_runProcessAsJob =
  describe "runProcessAsJob" $ do
    it "handles UTF-8 chars split across stdout chunks" $ do
      output <- runPythonScriptAndCollectTextOutput splitUtf8CharsToStdoutScript
      output `shouldBe` T.replicate 10 "€"

    it "handles UTF-8 chars split across stderr chunks" $ do
      output <- runPythonScriptAndCollectTextOutput splitUtf8CharsToStderrScript
      output `shouldBe` T.replicate 10 "€"

runPythonScriptAndCollectTextOutput :: String -> IO T.Text
runPythonScriptAndCollectTextOutput script = do
  chan <- newChan
  exitCode <- runProcessAsJob (P.proc "python3" ["-c", script]) J.Wasp chan
  exitCode `shouldBe` ExitSuccess
  reverseChunks <- collectJobTextOutputUntilExitReceived chan
  return $ T.concat $ reverse reverseChunks

splitUtf8CharsToStdoutScript :: String
splitUtf8CharsToStdoutScript =
  unlines
    [ "import sys, time",
      "out = sys.stdout.buffer",
      "for _ in range(10):",
      "  out.write(b'\\xe2'); out.flush(); time.sleep(0.005)",
      "  out.write(b'\\x82'); out.flush(); time.sleep(0.005)",
      "  out.write(b'\\xac'); out.flush(); time.sleep(0.005)"
    ]

splitUtf8CharsToStderrScript :: String
splitUtf8CharsToStderrScript =
  unlines
    [ "import sys, time",
      "out = sys.stderr.buffer",
      "for _ in range(10):",
      "  out.write(b'\\xe2'); out.flush(); time.sleep(0.005)",
      "  out.write(b'\\x82'); out.flush(); time.sleep(0.005)",
      "  out.write(b'\\xac'); out.flush(); time.sleep(0.005)"
    ]
