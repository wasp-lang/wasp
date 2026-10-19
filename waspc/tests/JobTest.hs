module JobTest where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (IOException, finally, onException, throwIO, try)
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified StrongPath as SP
import System.Directory (doesFileExist, removeFile)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Info
import System.Timeout (timeout)
import Test.Hspec (Spec, anyIOException, describe, expectationFailure, it, shouldBe, shouldReturn, shouldThrow)
import Test.Process.Util (ProcessId, isProcessAlive, killProcess, makeTempPath, readProcessId, waitUntil)
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

  describe "onProcessExit" $ do
    -- On Windows, the job doesn't keep running after the process exits, even
    -- if a child it left behind inherited its output, so this scenario can't
    -- be set up there.
    unless (System.Info.os == "mingw32") $
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

    it "emits all the output of a process that exited, even if the job is stopped right after" $ do
      processExited <- newEmptyMVar
      let process = node "process.stdout.write('x'.repeat(200000));"
          -- Passing the output on slowly leaves some of it unread when the
          -- process exits.
          passOutputOnSlowly = Job.observeOutput (\_ _ -> threadDelay 10000)
          job =
            Job.onProcessExit (\_ -> void $ tryPutMVar processExited ())
              $ passOutputOnSlowly
              $ Job.fromProc process
      result <-
        timeout (secondsToMicroSeconds 10)
          $ Job.run
          $ Job.captureOutput
          $ Job.race job (liftIO $ takeMVar processExited)
      T.length . snd <$> result `shouldBe` Just 200000

  describe "race" $ do
    it "returns the result of the job that finishes first" $ do
      let slowJob = liftIO $ threadDelay $ secondsToMicroSeconds 10
      timeout (secondsToMicroSeconds 5) (Job.run $ Job.race slowJob (return ("fast" :: String)))
        `shouldReturn` Just (Right "fast")

    it "stops the process of the job that didn't finish" $ do
      pidPath <- makeTempPath "wasp-race-process"
      -- The process exits by itself after a while, so that the test fails
      -- instead of hanging if stopping it doesn't work.
      let slowProcess = nodeWithArgs (writePidFileScript <> "setTimeout(() => {}, 10000);") [pidPath]
          jobThatFinishesOnceProcessStarts = liftIO $ waitUntil "the process starts" $ doesFileExist pidPath
      ( do
          result <-
            timeout (secondsToMicroSeconds 5)
              $ Job.run
              $ Job.race (Job.fromProc slowProcess) jobThatFinishesOnceProcessStarts
          case result of
            Just (Right ()) -> do
              pid <- readProcessId pidPath
              waitUntil "the process exits" $ not <$> isProcessAlive pid
            _ -> expectationFailure $ "Expected the race to finish, but got: " <> show result
        )
        `finally` mapM_ removeIfExists [pidPath, pidPath <> ".tmp"]

  describe "keepReplacing" $ do
    it "keeps each job running until the next one is produced, then stops it" $ do
      events <- newIORef []
      nextJobNumber <- newEmptyMVar
      let record event = atomicModifyIORef' events $ \recorded -> (recorded <> [event], ())
          numberedJob :: Int -> Job.Job ()
          numberedJob n = liftIO $ do
            record $ "start " <> show n
            forever (threadDelay maxBound) `finally` record ("stop " <> show n)
          next = do
            n <- liftIO $ takeMVar nextJobNumber
            liftIO $ record $ "produce " <> show n
            return $ Just $ numberedJob n
      Async.withAsync (Job.run $ Job.keepReplacing next $ numberedJob 0) $ \_ -> do
        waitUntil "the first job starts" $ elem "start 0" <$> readIORef events
        putMVar nextJobNumber 1
        waitUntil "the second job starts" $ elem "start 1" <$> readIORef events
        putMVar nextJobNumber 2
        waitUntil "the last job starts" $ elem "start 2" <$> readIORef events
      readIORef events
        `shouldReturn` ["start 0", "produce 1", "stop 0", "start 1", "produce 2", "stop 1", "start 2", "stop 2"]

    it "keeps the current job when the next one is nothing" $ do
      events <- newIORef []
      nextJobNumber <- newEmptyMVar
      let record event = atomicModifyIORef' events $ \recorded -> (recorded <> [event], ())
          numberedJob :: Int -> Job.Job ()
          numberedJob n = liftIO $ do
            record $ "start " <> show n
            forever (threadDelay maxBound) `finally` record ("stop " <> show n)
          next = do
            n <- liftIO $ takeMVar nextJobNumber
            liftIO $ record $ "produce " <> maybe "nothing" show n
            return $ numberedJob <$> n
      Async.withAsync (Job.run $ Job.keepReplacing next $ numberedJob 0) $ \_ -> do
        waitUntil "the first job starts" $ elem "start 0" <$> readIORef events
        putMVar nextJobNumber Nothing
        waitUntil "nothing is produced" $ elem "produce nothing" <$> readIORef events
        putMVar nextJobNumber $ Just 1
        waitUntil "the second job starts" $ elem "start 1" <$> readIORef events
      readIORef events
        `shouldReturn` ["start 0", "produce nothing", "produce 1", "stop 0", "start 1", "stop 1"]

    it "keeps waiting for the next job when the current one finishes" $ do
      events <- newIORef []
      nextJobNumber <- newEmptyMVar
      let record event = atomicModifyIORef' events $ \recorded -> (recorded <> [event], ())
          numberedJob :: Int -> Job.Job ()
          numberedJob n = liftIO $ record $ "run " <> show n
          next = do
            n <- liftIO $ takeMVar nextJobNumber `onException` record "next stopped"
            return $ Just $ numberedJob n
      Async.withAsync (Job.run $ Job.keepReplacing next $ numberedJob 0) $ \_ -> do
        waitUntil "the first job finishes" $ elem "run 0" <$> readIORef events
        putMVar nextJobNumber 1
        waitUntil "the second job finishes" $ elem "run 1" <$> readIORef events
      readIORef events `shouldReturn` ["run 0", "run 1", "next stopped"]

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

    it "stops the process and fails if emitting its output fails" $ do
      pidPath <- makeTempPath "wasp-failing-output-process"
      -- The process exits by itself after a while, so that the test fails
      -- instead of hanging if stopping it doesn't work.
      let process = nodeWithArgs (writePidFileScript <> "process.stdout.write('out'); setTimeout(() => {}, 10000);") [pidPath]
          failOnOutput = Job.observeOutput $ \_ _ -> throwIO $ userError "Emitting output failed"
      ( do
          result <- timeout (secondsToMicroSeconds 5) $ tryIOException $ Job.run $ failOnOutput $ Job.fromProc process
          case result of
            Just (Left _) -> do
              pid <- readProcessId pidPath
              waitUntil "the process exits" $ not <$> isProcessAlive pid
            _ -> expectationFailure $ "Expected the job to fail, but got: " <> show result
        )
        `finally` mapM_ removeIfExists [pidPath, pidPath <> ".tmp"]

    it "doesn't stop another job's process when one job is stopped" $
      withRunningProcess "" $ \first firstPid ->
        withRunningProcess "" $ \second secondPid -> do
          Async.cancel first
          waitUntil "the first process exits" $ not <$> isProcessAlive firstPid
          Async.poll second >>= \case
            Nothing -> isProcessAlive secondPid `shouldReturn` True
            Just _ -> expectationFailure "Stopping one job stopped another job's process"

  describe "npmBinProc" $
    it "fails if the executable isn't in any node_modules/.bin directory" $
      withSystemTempDirectory "wasp-job-test" $ \tempDir -> do
        let dir = fromJust $ SP.parseAbsDir tempDir
        Job.run (Job.npmBinProc dir "missing-tool" [])
          `shouldThrow` anyIOException

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
                writePidFileScript,
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

-- | Writes the process's ID to the file at the path given as its first
-- argument, all at once, so that it's never read half-written.
writePidFileScript :: String
writePidFileScript =
  unlines
    [ "const pidPath = process.argv[1];",
      "require('node:fs').writeFileSync(pidPath + '.tmp', String(process.pid));",
      "require('node:fs').renameSync(pidPath + '.tmp', pidPath);"
    ]

tryIOException :: IO a -> IO (Either IOException a)
tryIOException = try

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
