module Job.ProcessTest where

import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Process as JobProcess
import Wasp.Util (secondsToMicroSeconds)

spec_JobProcess :: Spec
spec_JobProcess = do
  describe "JobProcess.run" $ do
    it "returns a nonzero child exit for explicit handling" $ do
      let action = do
            exitCode <- JobProcess.run $ node "process.exit(7)"
            liftIO $ exitCode `shouldBe` ExitFailure 7
      runJob ignoreOutput action `shouldReturn` ExitSuccess

    it "forwards stdout and stderr to the job's sink" $ do
      chunks <- newMVar []
      let printer _ stream output = modifyMVar_ chunks $ return . ((stream, output) :)
          action = JobProcess.run_ $ node "process.stdout.write('out'); process.stderr.write('err');"
      runJob printer action `shouldReturn` ExitSuccess
      sort <$> readMVar chunks `shouldReturn` [(Job.Stdout, "out"), (Job.Stderr, "err")]

    it "gives an empty stdin to a process that asks for a pipe" $ do
      let readsStdinToEnd = node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
          action = JobProcess.run_ readsStdinToEnd {P.std_in = P.CreatePipe}
      timeout (secondsToMicroSeconds 10) (runJob ignoreOutput action)
        `shouldReturn` Just (ExitFailure 3)

  describe "JobProcess.run_" $ do
    it "fails the job on a nonzero child exit" $ do
      runJob ignoreOutput (JobProcess.run_ $ node "process.exit(7)")
        `shouldReturn` ExitFailure 7

node :: String -> P.CreateProcess
node script = P.proc "node" ["-e", script]

ignoreOutput :: Job.Printer
ignoreOutput _ _ _ = return ()

-- | Runs the job and returns the exit code it finished with.
runJob :: Job.Printer -> Job.Job () -> IO ExitCode
runJob printer job = either (ExitFailure . Job.jobFailureExitCode) (const ExitSuccess) <$> Job.runJob printer job
