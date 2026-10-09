module JobTest where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)
import Test.Process.Util (ProcessId, isProcessAlive, killProcess, makeTempPath, parseProcessId, readProcessId, waitUntil)
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

  describe "onProcessExit" $
    it "calls the action as soon as the process exits, before all of its output is emitted" $ do
      childPidPath <- makeTempPath "wasp-child-pid"
      processExit <- newEmptyMVar
      -- The process exits right away, but leaves behind a child that keeps its
      -- output open.
      let process =
            nodeWithArgs
              ( unlines
                  [ "const fs = require('node:fs');",
                    "const { spawn } = require('node:child_process');",
                    "const child = spawn(process.execPath, ['-e', 'setInterval(() => {}, 1000)'], { stdio: 'inherit' });",
                    "fs.writeFileSync(process.argv[1], String(child.pid));",
                    "process.exit(3);"
                  ]
              )
              [childPidPath]
      Async.withAsync
        (Job.run $ Job.onProcessExit (putMVar processExit) $ Job.fromProc process)
        ( \running -> do
            timeout (secondsToMicroSeconds 5) (takeMVar processExit) `shouldReturn` Just (ExitFailure 3)
            childPid <- readProcessId childPidPath
            ( do
                Async.poll running >>= \case
                  Nothing -> return ()
                  Just _ -> expectationFailure "The job finished while the process's output was still open"
                killProcess childPid
                timeout (secondsToMicroSeconds 5) (Async.wait running) `shouldReturn` Just (ExitFailure 3)
              )
              `finally` killProcess childPid
        )
        `finally` removeIfExists childPidPath

  describe "race" $ do
    it "returns the result of the job that finishes first" $ do
      let slowJob = liftIO $ threadDelay $ secondsToMicroSeconds 10
      timeout (secondsToMicroSeconds 5) (Job.run $ Job.race slowJob (return ("fast" :: String)))
        `shouldReturn` Just (Right "fast")

    it "stops the process of the job that didn't finish, and waits for it to exit" $ do
      processStarted <- newEmptyMVar
      -- The process exits by itself after a while, so that the test fails
      -- instead of hanging if stopping it doesn't work.
      let slowProcess = node "console.log(process.pid); setTimeout(() => {}, 10000)"
          slowJob = Job.onOutput (void $ tryPutMVar processStarted ()) $ Job.fromProc slowProcess
          jobThatFinishesOnceProcessStarts = liftIO $ takeMVar processStarted
      result <-
        timeout (secondsToMicroSeconds 5)
          $ Job.run
          $ Job.captureOutput
          $ Job.race slowJob jobThatFinishesOnceProcessStarts
      case result of
        Just (Right (), output) -> case parseProcessId $ T.unpack output of
          Just pid -> isProcessAlive pid `shouldReturn` False
          Nothing -> expectationFailure $ "Invalid process ID: " <> T.unpack output
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

    it "emits all the output before returning the exit code" $ do
      let process = node "process.stdout.write('x'.repeat(200000)); process.exitCode = 7;"
      runProcessJob (Job.captureOutput $ Job.fromProc process)
        `shouldReturn` Just (ExitFailure 7, T.replicate 200000 "x")

    it "decodes UTF-8 output split across chunks, replacing incomplete characters" $ do
      let euroSignCount = 40000 :: Int
          process =
            node $
              "process.stdout.write(Buffer.concat([Buffer.from('\8364'.repeat("
                <> show euroSignCount
                <> ")), Buffer.from([0xe2])]));"
      runProcessJob (Job.captureOutput $ Job.fromProc process)
        `shouldReturn` Just (ExitSuccess, T.replicate euroSignCount "\8364" <> "\65533")

    it "gives an empty stdin to a process that isn't interactive" $ do
      let readsStdinToEnd = node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
      runProcessJob (Job.fromProc readsStdinToEnd)
        `shouldReturn` Just (ExitFailure 3)

    it "asks the process to stop before killing it" $ do
      stoppedGracefullyPath <- makeTempPath "wasp-stopped-gracefully"
      let stopGracefully =
            "process.on('SIGTERM', () => { require('node:fs').writeFileSync("
              <> show stoppedGracefullyPath
              <> ", ''); process.exit(0); });"
      withRunningProcess
        stopGracefully
        ( \running _ -> do
            Async.cancel running
            doesFileExist stoppedGracefullyPath `shouldReturn` True
        )
        `finally` removeIfExists stoppedGracefullyPath

    it "kills a process that doesn't stop when asked to" $ do
      withRunningProcess "process.on('SIGTERM', () => {});" $ \running pid -> do
        timeout (secondsToMicroSeconds 6) (Async.cancel running) `shouldReturn` Just ()
        isProcessAlive pid `shouldReturn` False

    it "doesn't stop another job's process when one job is stopped" $
      withRunningProcess "" $ \first _ ->
        withRunningProcess "" $ \second secondPid -> do
          Async.cancel first
          Async.poll second >>= \case
            Nothing -> isProcessAlive secondPid `shouldReturn` True
            Just _ -> expectationFailure "Stopping one job stopped another job's process"

-- | Fails the test instead of hanging it if the process doesn't finish.
runProcessJob :: Job.Job a -> IO (Maybe a)
runProcessJob = timeout (secondsToMicroSeconds 10) . Job.run

node :: String -> Job.CreateJobProcess
node script = nodeWithArgs script []

nodeWithArgs :: String -> [String] -> Job.CreateJobProcess
nodeWithArgs script args = Job.proc "node" $ ["-e", script] <> args

-- | Runs a process as a job while the action runs, once the process has
-- started. The process runs the given script, and then keeps running until
-- it's stopped.
withRunningProcess :: String -> (Async.Async ExitCode -> ProcessId -> IO ()) -> IO ()
withRunningProcess script action = do
  pidPath <- makeTempPath "wasp-running-process"
  let process =
        nodeWithArgs
          ( unlines
              [ script,
                "const fs = require('node:fs');",
                "const pidPath = process.argv[1];",
                "fs.writeFileSync(pidPath + '.tmp', String(process.pid));",
                "fs.renameSync(pidPath + '.tmp', pidPath);",
                "setInterval(() => {}, 1000);"
              ]
          )
          [pidPath]
  Async.withAsync
    (Job.run $ Job.fromProc process)
    ( \running -> do
        waitUntil "the process starts" $ doesFileExist pidPath
        pid <- readProcessId pidPath
        action running pid `finally` killProcess pid
    )
    `finally` mapM_ removeIfExists [pidPath, pidPath <> ".tmp"]

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
