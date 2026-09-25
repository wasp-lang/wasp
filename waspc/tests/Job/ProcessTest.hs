module Job.ProcessTest where

import Control.Concurrent (newChan)
import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import qualified Wasp.Job as J
import qualified Wasp.Job.Kind as Kind
import qualified Wasp.Job.Process as JobProcess
import Wasp.Process (InputMode (NoInput))

spec_runProcess :: Spec
spec_runProcess =
  describe "JobProcess.runChecked NoInput" $ do
    it "fails the Job on a nonzero child exit" $ do
      chan <- newChan
      let action = JobProcess.runChecked NoInput $ P.proc "node" ["-e", "process.exit(7)"]
      J.runJob Kind.Wasp action chan `shouldReturn` ExitFailure 7

    it "can return a nonzero child exit for explicit handling" $ do
      chan <- newChan
      let action = do
            exitCode <- JobProcess.runReturningExitCode NoInput $ P.proc "node" ["-e", "process.exit(7)"]
            liftIO $ exitCode `shouldBe` ExitFailure 7
      J.runJob Kind.Wasp action chan `shouldReturn` ExitSuccess
