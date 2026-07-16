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
      output <- runNodeScriptAndCollectTextOutput splitUtf8CharsToStdoutScript
      output `shouldBe` T.replicate 10 "€"

    it "handles UTF-8 chars split across stderr chunks" $ do
      output <- runNodeScriptAndCollectTextOutput splitUtf8CharsToStderrScript
      output `shouldBe` T.replicate 10 "€"

runNodeScriptAndCollectTextOutput :: String -> IO T.Text
runNodeScriptAndCollectTextOutput script = do
  chan <- newChan
  exitCode <- runProcessAsJob (CP.proc "node" ["-e", script]) J.Wasp chan
  exitCode `shouldBe` ExitSuccess
  reverseChunks <- collectJobTextOutputUntilExitReceived chan
  return $ T.concat $ reverse reverseChunks

splitUtf8CharsToStdoutScript :: String
splitUtf8CharsToStdoutScript =
  unlines
    [ "const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));",
      "const out = process.stdout;",
      "const writeSplitChars = async () => {",
      "  for (let i = 0; i < 10; i++) {",
      "    out.write(Buffer.from([0xe2])); await sleep(5);",
      "    out.write(Buffer.from([0x82])); await sleep(5);",
      "    out.write(Buffer.from([0xac])); await sleep(5);",
      "  }",
      "};",
      "writeSplitChars().catch((err) => {",
      "  process.stderr.write(String(err));",
      "  process.exit(1);",
      "});"
    ]

splitUtf8CharsToStderrScript :: String
splitUtf8CharsToStderrScript =
  unlines
    [ "const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));",
      "const out = process.stderr;",
      "const writeSplitChars = async () => {",
      "  for (let i = 0; i < 10; i++) {",
      "    out.write(Buffer.from([0xe2])); await sleep(5);",
      "    out.write(Buffer.from([0x82])); await sleep(5);",
      "    out.write(Buffer.from([0xac])); await sleep(5);",
      "  }",
      "};",
      "writeSplitChars().catch((err) => {",
      "  process.stderr.write(String(err));",
      "  process.exit(1);",
      "});"
    ]
