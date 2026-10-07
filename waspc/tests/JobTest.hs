module JobTest where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldReturn)
import qualified Wasp.Job as Job
import Wasp.Util (secondsToMicroSeconds)

spec_Job :: Spec
spec_Job = do
  describe "captureOutput" $ do
    it "collects stdout and stderr in the order they were emitted" $ do
      let job = do
            Job.emitJobOutput Job.Stdout "first "
            Job.emitJobOutput Job.Stderr "second "
            Job.emitJobOutput Job.Stdout "last"
      runJob (Job.captureOutput job) `shouldReturn` Right ((), "first second last")

    it "doesn't pass the output on" $ do
      outputCount <- newIORef (0 :: Int)
      let job =
            Job.onOutput (modifyIORef' outputCount (+ 1))
              $ Job.captureOutput
              $ Job.emitJobOutput Job.Stdout "captured"
      _ <- runJob job
      readIORef outputCount `shouldReturn` 0

  describe "onOutput" $ do
    it "calls the action for every output" $ do
      outputCount <- newIORef (0 :: Int)
      let job =
            Job.captureOutput $
              Job.onOutput (modifyIORef' outputCount (+ 1)) $ do
                Job.emitJobOutput Job.Stdout "first"
                Job.emitJobOutput Job.Stderr "second"
      _ <- runJob job
      readIORef outputCount `shouldReturn` 2

  describe "maybeFailWith" $ do
    it "fails the job with the error for the exit code" $ do
      let job = do
            Job.maybeFailWith failOnExitFailure $ return $ ExitFailure 7
            Job.emitJobOutput Job.Stdout "after failure"
      runJob job `shouldReturn` Left "Failed with 7"

    it "doesn't fail the job when there is no error for the exit code" $ do
      runJob (Job.maybeFailWith failOnExitFailure $ return ExitSuccess)
        `shouldReturn` Right ()

  describe "race" $ do
    it "returns the result of the job that finishes first" $ do
      let slowJob = liftIO $ threadDelay $ secondsToMicroSeconds 10
      timeout (secondsToMicroSeconds 5) (runJob $ Job.race slowJob (return ("fast" :: String)))
        `shouldReturn` Just (Right (Right "fast"))

    it "fails if the job that finishes first fails" $ do
      let slowJob = liftIO $ threadDelay $ secondsToMicroSeconds 10
          failingJob = Job.maybeFailWith failOnExitFailure $ return $ ExitFailure 7
      timeout (secondsToMicroSeconds 5) (runJob $ Job.race slowJob failingJob)
        `shouldReturn` Just (Left "Failed with 7")

    it "stops the process of the job that didn't finish" $ do
      -- The process exits by itself after a while, so that the test fails
      -- instead of hanging if stopping it doesn't work.
      let slowProcess = (node "setTimeout(() => {}, 10000)") {P.create_group = True}
      timeout (secondsToMicroSeconds 5) (runJob $ Job.race (Job.fromProc slowProcess) (return ()))
        `shouldReturn` Just (Right (Right ()))

  describe "fromProc" $ do
    it "returns the exit code of the process without failing the job" $ do
      runProcessJob (Job.fromProc $ node "process.exit(7)") `shouldReturn` Just (Right (ExitFailure 7))

    it "emits the process's stdout and stderr" $ do
      let process = node "process.stdout.write('out'); setTimeout(() => process.stderr.write('err'), 100);"
      runProcessJob (Job.captureOutput $ Job.fromProc process)
        `shouldReturn` Just (Right (ExitSuccess, "outerr"))

    it "gives an empty stdin to a process that asks for a pipe" $ do
      let readsStdinToEnd = node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
      runProcessJob (Job.fromProc readsStdinToEnd {P.std_in = P.CreatePipe})
        `shouldReturn` Just (Right (ExitFailure 3))

runJob :: Job.Job String a -> IO (Either String a)
runJob = runExceptT . Job.run

-- | Fails the test instead of hanging it if the process doesn't finish.
runProcessJob :: Job.Job String a -> IO (Maybe (Either String a))
runProcessJob = timeout (secondsToMicroSeconds 10) . runJob

failOnExitFailure :: ExitCode -> Maybe String
failOnExitFailure ExitSuccess = Nothing
failOnExitFailure (ExitFailure code) = Just $ "Failed with " <> show code

node :: String -> P.CreateProcess
node script = P.proc "node" ["-e", script]
