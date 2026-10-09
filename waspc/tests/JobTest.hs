module JobTest where

import Control.Concurrent (newEmptyMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import qualified System.Info
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)
import qualified Wasp.Job as Job
import Wasp.Util (secondsToMicroSeconds)

spec_Job :: Spec
spec_Job = do
  describe "captureOutput" $ do
    it "collects stdout and stderr in the order they were emitted" $ do
      let job = do
            Job.emitOutput Job.Stdout "first "
            Job.emitOutput Job.Stderr "second "
            Job.emitOutput Job.Stdout "last"
      Job.run (Job.captureOutput job) `shouldReturn` ((), "first second last")

    it "doesn't pass the output on" $ do
      outputCount <- newIORef (0 :: Int)
      let job =
            Job.onOutput (modifyIORef' outputCount (+ 1))
              $ Job.captureOutput
              $ Job.emitOutput Job.Stdout "captured"
      _ <- Job.run job
      readIORef outputCount `shouldReturn` 0

  describe "onOutput" $ do
    it "calls the action for every output" $ do
      outputCount <- newIORef (0 :: Int)
      let job =
            Job.captureOutput $
              Job.onOutput (modifyIORef' outputCount (+ 1)) $ do
                Job.emitOutput Job.Stdout "first"
                Job.emitOutput Job.Stderr "second"
      _ <- Job.run job
      readIORef outputCount `shouldReturn` 2

  describe "race" $ do
    it "returns the result of the job that finishes first" $ do
      let slowJob = liftIO $ threadDelay $ secondsToMicroSeconds 10
      timeout (secondsToMicroSeconds 5) (Job.run $ Job.race slowJob (return ("fast" :: String)))
        `shouldReturn` Just (Right "fast")

    -- On Windows, stopping a job waits for its process to exit by itself,
    -- because reading the process's output can't be interrupted there.
    unless (System.Info.os == "mingw32") $
      it "stops the process of the job that didn't finish" $ do
        processStarted <- newEmptyMVar
        -- The process exits by itself after a while, so that the test fails
        -- instead of hanging if stopping it doesn't work.
        let slowProcess = (node "console.log(process.pid); setTimeout(() => {}, 10000)") {P.create_group = True}
            slowJob = Job.onOutput (void $ tryPutMVar processStarted ()) $ Job.fromProc slowProcess
            jobThatFinishesOnceProcessStarts = liftIO $ takeMVar processStarted
        result <-
          timeout (secondsToMicroSeconds 5)
            $ Job.run
            $ Job.captureOutput
            $ Job.race slowJob jobThatFinishesOnceProcessStarts
        case result of
          Just (Right (), output) ->
            waitForProcessToExit (read $ T.unpack output) `shouldReturn` True
          _ -> expectationFailure $ "Expected the race to finish, but got: " <> show result

  describe "andThen" $ do
    it "runs the second job if the first one succeeds" $ do
      let job =
            (Job.emitOutput Job.Stdout "first " >> return ExitSuccess)
              `Job.andThen` (Job.emitOutput Job.Stdout "second" >> return (ExitFailure 2))
      Job.run (Job.captureOutput job) `shouldReturn` (ExitFailure 2, "first second")

    it "doesn't run the second job if the first one fails" $ do
      let job =
            (Job.emitOutput Job.Stdout "first" >> return (ExitFailure 1))
              `Job.andThen` (Job.emitOutput Job.Stdout " second" >> return ExitSuccess)
      Job.run (Job.captureOutput job) `shouldReturn` (ExitFailure 1, "first")

  describe "fromProc" $ do
    it "returns the exit code of the process" $ do
      runProcessJob (Job.fromProc $ node "process.exit(7)") `shouldReturn` Just (ExitFailure 7)

    it "emits the process's stdout and stderr" $ do
      let process = node "process.stdout.write('out'); setTimeout(() => process.stderr.write('err'), 100);"
      runProcessJob (Job.captureOutput $ Job.fromProc process)
        `shouldReturn` Just (ExitSuccess, "outerr")

    it "gives an empty stdin to a process that asks for a pipe" $ do
      let readsStdinToEnd = node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
      runProcessJob (Job.fromProc readsStdinToEnd {P.std_in = P.CreatePipe})
        `shouldReturn` Just (ExitFailure 3)

-- | Fails the test instead of hanging it if the process doesn't finish.
runProcessJob :: Job.Job a -> IO (Maybe a)
runProcessJob = timeout (secondsToMicroSeconds 10) . Job.run

node :: String -> P.CreateProcess
node script = P.proc "node" ["-e", script]

-- | Returns whether the process with the given ID exits within 5 seconds.
waitForProcessToExit :: Int -> IO Bool
waitForProcessToExit pid = go (50 :: Int)
  where
    go 0 = return False
    go attemptsLeft = do
      (exitCode, _, _) <- P.readCreateProcessWithExitCode (node $ "process.kill(" <> show pid <> ", 0)") ""
      if exitCode /= ExitSuccess
        then return True
        else threadDelay (secondsToMicroSeconds 1 `div` 10) >> go (attemptsLeft - 1)
