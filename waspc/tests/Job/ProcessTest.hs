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
      Job.runJob ignoreOutput action `shouldReturn` ExitSuccess

    it "forwards stdout and stderr to the job's sink" $ do
      chunks <- newMVar []
      let sink stream output = modifyMVar_ chunks $ return . ((stream, output) :)
          action = JobProcess.run_ $ node "process.stdout.write('out'); process.stderr.write('err');"
      Job.runJob sink action `shouldReturn` ExitSuccess
      sort <$> readMVar chunks `shouldReturn` [(Job.Stdout, "out"), (Job.Stderr, "err")]

    it "gives an empty stdin to a process that asks for a pipe" $ do
      let readsStdinToEnd = node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
          action = JobProcess.run_ readsStdinToEnd {P.std_in = P.CreatePipe}
      timeout (secondsToMicroSeconds 10) (Job.runJob ignoreOutput action)
        `shouldReturn` Just (ExitFailure 3)

  describe "JobProcess.run_" $ do
    it "fails the job on a nonzero child exit" $ do
      Job.runJob ignoreOutput (JobProcess.run_ $ node "process.exit(7)")
        `shouldReturn` ExitFailure 7

node :: String -> P.CreateProcess
node script = P.proc "node" ["-e", script]

ignoreOutput :: Job.Sink
ignoreOutput _ _ = return ()
