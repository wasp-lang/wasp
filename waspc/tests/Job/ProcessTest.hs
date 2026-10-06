module Job.ProcessTest where

import System.Exit (ExitCode (..))
import qualified System.Process as P
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import qualified Wasp.Job.Output as Output
import qualified Wasp.Job.Process as JobProcess
import Wasp.Process (InputMode (NoInput))

spec_runProcess :: Spec
spec_runProcess =
  describe "JobProcess.run" $ do
    it "returns the child's exit code" $ do
      let job = JobProcess.run NoInput $ P.proc "node" ["-e", "process.exit(7)"]
      Output.runAndCaptureOutput job `shouldReturn` (ExitFailure 7, "")

    it "streams the child's stdout and stderr" $ do
      let job =
            JobProcess.run NoInput $
              P.proc "node" ["-e", "process.stdout.write('out'); process.stderr.write('err')"]
      (exitCode, output) <- Output.runAndCaptureOutput job
      exitCode `shouldBe` ExitSuccess
      output `shouldSatisfy` (`elem` ["outerr", "errout"])
