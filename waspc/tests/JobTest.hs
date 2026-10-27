module JobTest where

import Control.Concurrent (newEmptyMVar, takeMVar, threadDelay, tryPutMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import qualified System.Info
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)
import Wasp.Env (setEnvVars)
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
            Job.observeOutput (\_ _ -> modifyIORef' outputCount (+ 1))
              $ Job.captureOutput
              $ Job.emitOutput Job.Stdout "captured"
      _ <- Job.run job
      readIORef outputCount `shouldReturn` 0

  describe "observeOutput" $ do
    it "calls the action with every output, and passes it on" $ do
      tappedOutputs <- newIORef []
      let job =
            Job.captureOutput $
              Job.observeOutput (\outputType output -> modifyIORef' tappedOutputs ((outputType, output) :)) $ do
                Job.emitOutput Job.Stdout "first"
                Job.emitOutput Job.Stderr "second"
      Job.run job `shouldReturn` ((), "firstsecond")
      reverse <$> readIORef tappedOutputs `shouldReturn` [(Job.Stdout, "first"), (Job.Stderr, "second")]

  describe "race" $ do
    it "returns the result of the job that finishes first" $ do
      let slowJob = liftIO $ threadDelay $ secondsToMicroSeconds 10
      timeout (secondsToMicroSeconds 5) (Job.run $ Job.race slowJob (return ("fast" :: String)))
        `shouldReturn` Just (Right "fast")

    -- On Windows, stopping a job waits for its process to exit by itself,
    -- because reading the process's output can't be interrupted there, and it
    -- doesn't stop the process's descendants.
    unless (System.Info.os == "mingw32") $ do
      it "stops the process of the job that didn't finish" $
        -- The process exits by itself after a while, so that the test fails
        -- instead of hanging if stopping it doesn't work.
        stopsAfterOutput (node "console.log(process.pid); setTimeout(() => {}, 10000)")

      -- Like with `npm -> sh -> command`, the grandchild doesn't get the signal
      -- its grandparent gets, and keeps running after its ancestors exit,
      -- unless it's stopped too.
      it "stops the descendants of the process of the job that didn't finish"
        $ stopsAfterOutput
        $ nodeWithArgs spawnChildScript
        $ nodeArgs spawnChildScript
        $ nodeArgs "console.log(process.pid); setTimeout(() => {}, 10000)" []

      it "doesn't stop the processes of other jobs" $ do
        otherProcessStarted <- newEmptyMVar
        let otherJob =
              Job.captureOutput
                $ Job.observeOutput (\_ _ -> void $ tryPutMVar otherProcessStarted ())
                $ Job.fromProc
                $ node "console.log(process.pid); setTimeout(() => {}, 10000)"
        Async.withAsync (Job.run otherJob) $ \otherJobRun -> do
          takeMVar otherProcessStarted
          stopsAfterOutput $ node "console.log(process.pid); setTimeout(() => {}, 10000)"
          Async.poll otherJobRun >>= \case
            Nothing -> return ()
            Just _ -> expectationFailure "Stopping one job stopped another job's process"

  describe "fromProc" $ do
    it "returns the exit code of the process" $ do
      runProcessJob (Job.fromProc $ node "process.exit(7)") `shouldReturn` Just (ExitFailure 7)

    it "emits the process's stdout and stderr" $ do
      let process = node "process.stdout.write('out'); setTimeout(() => process.stderr.write('err'), 100);"
      runProcessJob (Job.captureOutput $ Job.fromProc process)
        `shouldReturn` Just (ExitSuccess, "outerr")

    it "gives the process Wasp's env vars, overridden by the ones set on it" $ do
      let inheritedVar = "WASP_JOB_TEST_INHERITED"
          overriddenVar = "WASP_JOB_TEST_OVERRIDDEN"
          process =
            setEnvVars [(overriddenVar, "set")]
              $ node
              $ "process.stdout.write(process.env." <> inheritedVar <> " + ' ' + process.env." <> overriddenVar <> ")"
      ( do
          setEnv inheritedVar "inherited"
          setEnv overriddenVar "inherited"
          runProcessJob (Job.captureOutput $ Job.fromProc process)
            `shouldReturn` Just (ExitSuccess, "inherited set")
        )
        `finally` mapM_ unsetEnv [inheritedVar, overriddenVar]

    it "gives an empty stdin to a process that isn't interactive" $ do
      let readsStdinToEnd = node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
      runProcessJob (Job.fromProc readsStdinToEnd)
        `shouldReturn` Just (ExitFailure 3)

-- | Fails the test instead of hanging it if the process doesn't finish.
runProcessJob :: Job.Job a -> IO (Maybe a)
runProcessJob = timeout (secondsToMicroSeconds 10) . Job.run

node :: String -> Job.CreateJobProcess
node script = nodeWithArgs script []

nodeWithArgs :: String -> [String] -> Job.CreateJobProcess
nodeWithArgs script args = Job.proc "node" $ nodeArgs script args

-- | `--` keeps `node` from reading the arguments as its own options.
nodeArgs :: String -> [String] -> [String]
nodeArgs script args = ["-e", script, "--"] <> args

-- | Runs `node` with the script's arguments, sharing its stdout, and keeps
-- running.
spawnChildScript :: String
spawnChildScript =
  "require('node:child_process').spawn(process.execPath, process.argv.slice(1), { stdio: 'inherit' }); setTimeout(() => {}, 10000)"

-- | Races a job running the process against a job that finishes once the
-- process emits some output, which must be the ID of the process to check, and
-- expects that process to exit afterwards.
stopsAfterOutput :: Job.CreateJobProcess -> IO ()
stopsAfterOutput process = do
  processStarted <- newEmptyMVar
  let slowJob = Job.observeOutput (\_ _ -> void $ tryPutMVar processStarted ()) $ Job.fromProc process
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

-- | Returns whether the process with the given ID exits within 5 seconds.
waitForProcessToExit :: Int -> IO Bool
waitForProcessToExit pid = go (50 :: Int)
  where
    go 0 = return False
    go attemptsLeft = do
      (exitCode, _, _) <- P.readProcessWithExitCode "node" ["-e", "process.kill(" <> show pid <> ", 0)"] ""
      if exitCode /= ExitSuccess
        then return True
        else threadDelay (secondsToMicroSeconds 1 `div` 10) >> go (attemptsLeft - 1)
