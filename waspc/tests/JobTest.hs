{-# LANGUAGE CPP #-}

module JobTest where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import qualified System.Info
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)
import Test.Process.Util (ProcessId, isPortAvailable, isProcessAlive, killProcess, makeTempPath, parseProcessId, readProcessId, waitUntil)
import qualified Wasp.Job as Job
import Wasp.Util (secondsToMicroSeconds)
#if !mingw32_HOST_OS
import qualified System.Posix.Process as Posix
import System.Posix.Types (ProcessGroupID)
import Test.Hspec (shouldNotBe)
import Test.Process.Util (processIdToPid)
#endif

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

  describe "onProcessExit" $ do
    -- On Windows, a process isn't seen to exit until the processes it left
    -- behind have exited too.
    unless isWindows $
      it "calls the action as soon as the process exits, before stopping the processes it left behind" $ do
        portRef <- newIORef ""
        processExit <- newEmptyMVar
        let recordProcessExit exitCode = do
              port <- readIORef portRef
              descendantIsRunning <- not <$> isPortAvailable port
              putMVar processExit (exitCode, descendantIsRunning)
        withListeningDescendant (Job.onProcessExit recordProcessExit) $ \running port exitRoot -> do
          writeIORef portRef port
          exitRoot
          timeout (secondsToMicroSeconds 3) (Async.wait running) `shouldReturn` Just ExitSuccess
          takeMVar processExit `shouldReturn` (ExitSuccess, True)

  describe "race" $ do
    it "returns the result of the job that finishes first" $ do
      let slowJob = liftIO $ threadDelay $ secondsToMicroSeconds 10
      timeout (secondsToMicroSeconds 5) (Job.run $ Job.race slowJob (return ("fast" :: String)))
        `shouldReturn` Just (Right "fast")

    it "stops the process of the job that didn't finish" $ do
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
          Just pid -> waitUntil "the process exits" $ not <$> isProcessAlive pid
          Nothing -> expectationFailure $ "Invalid process ID: " <> T.unpack output
        _ -> expectationFailure $ "Expected the race to finish, but got: " <> show result

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

    it "gives the process an empty stdin" $ do
      let readsStdinToEnd = node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
      runProcessJob (Job.fromProc readsStdinToEnd)
        `shouldReturn` Just (ExitFailure 3)

    it "stops the processes it started when the job is stopped" $
      withListeningDescendant id $ \running port _ -> do
        isPortAvailable port `shouldReturn` False
        timeout (secondsToMicroSeconds 3) (Async.cancel running) `shouldReturn` Just ()
        isPortAvailable port `shouldReturn` True

    unless isWindows $
      it "stops the processes it started once it exits" $
        withListeningDescendant id $ \running port exitRoot -> do
          exitRoot
          timeout (secondsToMicroSeconds 3) (Async.wait running) `shouldReturn` Just ExitSuccess
          isPortAvailable port `shouldReturn` True

    unless isWindows $
      it "interrupts the processes so they can exit gracefully before stopping them" $ do
        gracefulExitPath <- makeTempPath "wasp-graceful-exit"
        let exitGracefully =
              "process.on('SIGINT', () => { require('node:fs').writeFileSync("
                <> show gracefulExitPath
                <> ", ''); process.exit(0); });"
        ( withRunningProcess Job.fromProc exitGracefully $ \running _ -> do
            Async.cancel running
            doesFileExist gracefulExitPath `shouldReturn` True
          )
          `finally` removeIfExists gracefulExitPath

    it "forces a process that ignores interruption to stop" $ do
      let ignoreInterruption = "process.on('SIGINT', () => {}); process.on('SIGTERM', () => {});"
      withRunningProcess Job.fromProc ignoreInterruption $ \running pid -> do
        timeout (secondsToMicroSeconds 3) (Async.cancel running) `shouldReturn` Just ()
        isProcessAlive pid `shouldReturn` False

    it "doesn't stop another job's process when one job is stopped" $
      withRunningProcess Job.fromProc "" $ \first _ ->
        withRunningProcess Job.fromProc "" $ \second secondPid -> do
          Async.cancel first
          Async.poll second >>= \case
            Nothing -> isProcessAlive secondPid `shouldReturn` True
            Just _ -> expectationFailure "Stopping one job stopped another job's process"

    it "decodes UTF-8 output split across chunks, replacing incomplete characters" $ do
      let euroSignCount = 40000 :: Int
          process =
            node $
              "process.stdout.write(Buffer.concat([Buffer.from('\8364'.repeat("
                <> show euroSignCount
                <> ")), Buffer.from([0xe2])]));"
      runProcessJob (Job.captureOutput $ Job.fromProc process)
        `shouldReturn` Just (ExitSuccess, T.replicate euroSignCount "\8364" <> "\65533")

#if !mingw32_HOST_OS
    it "runs the process in its own process group" $ do
      waspGroup <- Posix.getProcessGroupID
      processGroup <- getProcessGroupOfProcessRunBy Job.fromProc
      processGroup `shouldNotBe` waspGroup

  describe "fromInteractiveProc" $ do
    it "runs the process in Wasp's process group" $ do
      waspGroup <- Posix.getProcessGroupID
      getProcessGroupOfProcessRunBy Job.fromInteractiveProc `shouldReturn` waspGroup
#endif

-- | Fails the test instead of hanging it if the process doesn't finish.
runProcessJob :: Job.Job a -> IO (Maybe a)
runProcessJob = timeout (secondsToMicroSeconds 10) . Job.run

node :: String -> P.CreateProcess
node script = nodeWithArgs script []

nodeWithArgs :: String -> [String] -> P.CreateProcess
nodeWithArgs script args = P.proc "node" $ ["-e", script] <> args

isWindows :: Bool
isWindows = System.Info.os == "mingw32"

-- | Runs a process with the given function while the action runs, once the
-- process has started. The process runs until it's stopped, after running the
-- given script.
withRunningProcess ::
  (P.CreateProcess -> Job.Job ExitCode) ->
  String ->
  (Async.Async ExitCode -> ProcessId -> IO ()) ->
  IO ()
withRunningProcess runProcess script action = do
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
    (Job.run $ runProcess process)
    ( \running -> do
        waitUntil "the process starts" $ doesFileExist pidPath
        pid <- readProcessId pidPath
        action running pid `finally` killProcess pid
    )
    `finally` mapM_ removeIfExists [pidPath, pidPath <> ".tmp"]

-- | Runs a process that starts a descendant listening on a port, and gives the
-- action the running job, the port, and an action that makes the root process
-- exit (while the descendant keeps running).
withListeningDescendant ::
  (Job.Job ExitCode -> Job.Job ExitCode) ->
  (Async.Async ExitCode -> String -> IO () -> IO ()) ->
  IO ()
withListeningDescendant modifyJob action = do
  portPath <- makeTempPath "wasp-isolated-child-port"
  rootExitPath <- makeTempPath "wasp-isolated-root-exit"
  Async.withAsync
    (Job.run $ modifyJob $ Job.fromProc $ nodeWithArgs descendantRootScript [listeningServerScript, portPath, rootExitPath])
    ( \running -> do
        waitUntil "the descendant listens" $ doesFileExist portPath
        port <- readFile portPath
        action running port $ writeFile rootExitPath ""
    )
    `finally` mapM_ removeIfExists [portPath, portPath <> ".tmp", rootExitPath]

descendantRootScript :: String
descendantRootScript =
  unlines
    [ "const fs = require('node:fs')",
      "const { spawn } = require('node:child_process')",
      "const [childScript, portPath, exitPath] = process.argv.slice(1)",
      "spawn(process.execPath, ['-e', childScript, portPath], { stdio: 'inherit' })",
      "setInterval(() => { if (fs.existsSync(exitPath)) process.exit(0) }, 10)"
    ]

listeningServerScript :: String
listeningServerScript =
  unlines
    [ "const fs = require('node:fs')",
      "const server = require('node:net').createServer()",
      "const portPath = process.argv[1]",
      "server.listen(0, '127.0.0.1', () => {",
      "  fs.writeFileSync(portPath + '.tmp', String(server.address().port))",
      "  fs.renameSync(portPath + '.tmp', portPath)",
      "})"
    ]

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

#if !mingw32_HOST_OS
getProcessGroupOfProcessRunBy :: (P.CreateProcess -> Job.Job ExitCode) -> IO ProcessGroupID
getProcessGroupOfProcessRunBy runProcess = do
  processGroup <- newEmptyMVar
  withRunningProcess runProcess "" $ \_ pid ->
    Posix.getProcessGroupIDOf (processIdToPid pid) >>= putMVar processGroup
  takeMVar processGroup
#endif
