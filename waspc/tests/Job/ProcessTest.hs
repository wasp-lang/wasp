{-# LANGUAGE CPP #-}

module Job.ProcessTest where

import Control.Concurrent (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally, fromException)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sort)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.Info (os)
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Test.Process.Util (isPortAvailable, isProcessAlive, killProcess, makeTempPath, readProcessId, waitUntil)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output as Output
import qualified Wasp.Job.Process as JobProcess
import Wasp.Util (secondsToMicroSeconds)
#if !mingw32_HOST_OS
import qualified System.Posix.Process as Posix
import System.Posix.Types (ProcessGroupID)
import Text.Read (readMaybe)
#endif

spec_JobProcess :: Spec
spec_JobProcess = do
  describe "JobProcess.run_" $ do
    it "fails the job on a nonzero child exit" $ do
      runJob ignoreOutput (JobProcess.run_ $ node "process.exit(7)")
        `shouldReturn` ExitFailure 7

  describe "JobProcess.spawn" $ do
    it "stops the process tree when the job is cancelled" $ do
      portFilePath <- makeTempPath "wasp-spawned-job-port"
      let action = do
            subprocess <- JobProcess.spawn $ node $ portOwningChildProcessScript portFilePath
            JobProcess.wait subprocess >>= Job.requireExitSuccess
      Async.withAsync
        (runJob ignoreOutput action)
        ( \running -> do
            waitUntil "job child port file" $ doesFileExist portFilePath
            port <- readFile portFilePath
            isPortAvailable port `shouldReturn` False
            Async.cancel running
            isPortAvailable port `shouldReturn` True
        )
        `finally` removeIfExists portFilePath

    -- TODO: Windows Job Objects need separate root/tree handles; native supervisor work is out of scope here.
    when (os /= "mingw32") $
      it "stops process-group descendants after the root process exits" $ do
        portFilePath <- makeTempPath "wasp-spawned-child-port"
        let action = do
              subprocess <- JobProcess.spawn $ node $ exitingRootWithPortOwningChildScript portFilePath
              port <- liftIO $ do
                waitUntil "child port file" $ doesFileExist portFilePath
                port <- readFile portFilePath
                timeout (secondsToMicroSeconds 5) (JobProcess.wait subprocess) `shouldReturn` Just ExitSuccess
                JobProcess.poll subprocess `shouldReturn` Just ExitSuccess
                isPortAvailable port `shouldReturn` False
                return port
              stopDuration <- timed $ JobProcess.stop subprocess
              liftIO $ do
                stopDuration `shouldSatisfy` (< maxAcceptableStopSeconds)
                isPortAvailable port `shouldReturn` True
        runJobSuccessfully action `finally` removeIfExists portFilePath

    when (os /= "mingw32") $
      it "interrupts the process so it can exit gracefully before being killed" $ do
        startedFilePath <- makeTempPath "wasp-spawned-started"
        gracefulExitFilePath <- makeTempPath "wasp-spawned-graceful-exit"
        let action = do
              subprocess <- JobProcess.spawn $ node $ gracefulProcessScript startedFilePath gracefulExitFilePath
              liftIO $ waitUntil "process start" $ doesFileExist startedFilePath
              JobProcess.stop subprocess
              liftIO $ waitUntil "graceful exit marker" $ doesFileExist gracefulExitFilePath
        runJobSuccessfully action
          `finally` mapM_ removeIfExists [startedFilePath, gracefulExitFilePath]

    it "kills a process that ignores graceful stop signals" $ do
      startedFilePath <- makeTempPath "wasp-spawned-stubborn"
      let action = do
            subprocess <- JobProcess.spawn $ node $ stubbornProcessScript startedFilePath
            liftIO $ waitUntil "process start" $ doesFileExist startedFilePath
            stopDuration <- timed $ JobProcess.stop subprocess
            liftIO $ do
              stopDuration `shouldSatisfy` (< maxAcceptableStopSeconds)
              rootExit <- timeout (secondsToMicroSeconds 5) $ JobProcess.wait subprocess
              rootExit `shouldSatisfy` isJust
      runJobSuccessfully action `finally` removeIfExists startedFilePath

    it "releases a descendant-owned port before stop returns" $ do
      portFilePath <- makeTempPath "wasp-spawned-port"
      let action = do
            subprocess <- JobProcess.spawn $ node $ portOwningChildProcessScript portFilePath
            port <- liftIO $ do
              waitUntil "child-owned port" $ doesFileExist portFilePath
              port <- readFile portFilePath
              isPortAvailable port `shouldReturn` False
              return port
            JobProcess.stop subprocess
            liftIO $ isPortAvailable port `shouldReturn` True
      runJobSuccessfully action `finally` removeIfExists portFilePath

    it "decodes chunk-split and incomplete UTF-8 output" $ do
      let euroSignCount = 40000 :: Int
          script =
            "process.stdout.write(Buffer.concat([Buffer.from('€'.repeat("
              <> show euroSignCount
              <> ")), Buffer.from([0xe2])]));"
          action = JobProcess.run_ $ node script
      Output.capturing (`runJob` action)
        `shouldReturn` (ExitSuccess, T.replicate euroSignCount "€" <> "�")

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

    it "forwards all output before returning the exit code" $ do
      let action = JobProcess.run_ $ node "process.stdout.write('x'.repeat(200000)); process.exitCode = 7;"
      Output.capturing (`runJob` action)
        `shouldReturn` (ExitFailure 7, T.replicate 200000 "x")

    it "gives commands an empty stdin" $ do
      let action = JobProcess.run_ $ node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
      timeout (secondsToMicroSeconds 10) (runJob ignoreOutput action)
        `shouldReturn` Just (ExitFailure 3)

    it "stops descendants on cancellation" $
      withListeningDescendant $ \running port _ -> do
        isPortAvailable port `shouldReturn` False
        timeout 3000000 (Async.cancel running) `shouldReturn` Just ()
        isPortAvailable port `shouldReturn` True

    when (os /= "mingw32") $
      it "stops descendants after the root exits" $
        withListeningDescendant $ \running port exitRoot -> do
          exitRoot
          timeout 3000000 (Async.wait running) `shouldReturn` Just ExitSuccess
          isPortAvailable port `shouldReturn` True

    it "forces a command that ignores graceful interruption to stop" $ do
      pidPath <- makeTempPath "wasp-stubborn-process"
      let script =
            unlines
              [ "const fs = require('node:fs');",
                "process.on('SIGINT', () => {});",
                "process.on('SIGTERM', () => {});",
                "const pidPath = " <> show pidPath <> ";",
                "fs.writeFileSync(pidPath + '.tmp', String(process.pid));",
                "fs.renameSync(pidPath + '.tmp', pidPath);",
                "setInterval(() => {}, 1000);"
              ]
      Async.withAsync
        (runJob ignoreOutput $ JobProcess.run_ $ node script)
        ( \running -> do
            waitUntil "stubborn process ready" $ doesFileExist pidPath
            pid <- readProcessId pidPath
            timeout 3000000 (Async.cancel running) `shouldReturn` Just ()
            Async.waitCatch running >>= \case
              Left exception -> case fromException exception of
                Just Async.AsyncCancelled -> return ()
                Nothing -> fail $ "Unexpected cleanup exception: " <> show exception
              Right _ -> fail "Expected cancellation"
            isProcessAlive pid `shouldReturn` False
        )
        `finally` do
          exists <- doesFileExist pidPath
          when exists $ do
            readProcessId pidPath >>= killProcess
            removeFile pidPath
          temporaryFileExists <- doesFileExist $ pidPath <> ".tmp"
          when temporaryFileExists $ removeFile $ pidPath <> ".tmp"

    it "does not stop another command when one is cancelled" $ do
      firstReady <- newEmptyMVar
      secondReady <- newEmptyMVar
      let action = JobProcess.run_ $ node "console.log('ready'); setInterval(() => {}, 1000);"
      Async.withAsync (runJob (\_ _ _ -> putMVar firstReady ()) action) $ \first ->
        Async.withAsync (runJob (\_ _ _ -> putMVar secondReady ()) action) $ \second -> do
          timeout 5000000 (takeMVar firstReady) `shouldReturn` Just ()
          timeout 5000000 (takeMVar secondReady) `shouldReturn` Just ()
          Async.cancel first
          Async.poll second >>= \case
            Nothing -> return ()
            Just _ -> fail "Cancelling one command stopped another command"

#if !mingw32_HOST_OS
    it "keeps interactive commands in the caller's group and isolates other commands" $ do
      parentGroup <- Posix.getProcessGroupID
      assertGroup parentGroup True
      assertGroup parentGroup False
#endif

node :: String -> P.CreateProcess
node script = nodeWithArgs script []

nodeWithArgs :: String -> [String] -> P.CreateProcess
nodeWithArgs script args = JobProcess.command "node" $ ["-e", script] <> args

ignoreOutput :: Job.Printer
ignoreOutput _ _ _ = return ()

-- | Runs the job and returns the exit code it finished with.
runJob :: Job.Printer -> Job.Job () -> IO ExitCode
runJob printer job = either (ExitFailure . Job.jobFailureExitCode) (const ExitSuccess) <$> Job.runJob printer job

runJobSuccessfully :: Job.Job () -> IO ()
runJobSuccessfully action = runJob ignoreOutput action `shouldReturn` ExitSuccess

timed :: (MonadIO m) => m () -> m Double
timed action = do
  startedAt <- liftIO getCurrentTime
  action
  finishedAt <- liftIO getCurrentTime
  return $ realToFrac $ finishedAt `diffUTCTime` startedAt

-- Covers graceful stop, hard-stop escalation, and polling slack.
maxAcceptableStopSeconds :: Double
maxAcceptableStopSeconds = 2

exitingRootWithPortOwningChildScript :: FilePath -> String
exitingRootWithPortOwningChildScript portFilePath =
  unlines
    [ "const { spawn } = require('node:child_process');",
      "const childScript = " <> show portOwningChildScript <> ";",
      "spawn(process.execPath, ['-e', childScript, " <> show portFilePath <> "], { stdio: 'inherit' });",
      "setTimeout(() => process.exit(0), 200);"
    ]

gracefulProcessScript :: FilePath -> FilePath -> String
gracefulProcessScript startedFilePath gracefulExitFilePath =
  unlines
    [ "const fs = require('node:fs');",
      "fs.writeFileSync(" <> show startedFilePath <> ", 'started');",
      "process.on('SIGINT', () => {",
      "  fs.writeFileSync(" <> show gracefulExitFilePath <> ", 'done');",
      "  process.exit(0);",
      "});",
      "setInterval(() => {}, 1000);"
    ]

stubbornProcessScript :: FilePath -> String
stubbornProcessScript startedFilePath =
  unlines
    [ "const fs = require('node:fs');",
      "fs.writeFileSync(" <> show startedFilePath <> ", 'started');",
      "process.on('SIGINT', () => {});",
      "process.on('SIGTERM', () => {});",
      "setInterval(() => {}, 1000);"
    ]

portOwningChildProcessScript :: FilePath -> String
portOwningChildProcessScript portFilePath =
  unlines
    [ "const { spawn } = require('node:child_process');",
      "const childScript = " <> show portOwningChildScript <> ";",
      "spawn(process.execPath, ['-e', childScript, " <> show portFilePath <> "], { stdio: 'inherit' });",
      "process.on('SIGINT', () => process.exit(0));",
      "setInterval(() => {}, 1000);"
    ]

portOwningChildScript :: String
portOwningChildScript =
  unlines
    [ "const fs = require('node:fs');",
      "const net = require('node:net');",
      "process.on('SIGINT', () => {});",
      "const server = net.createServer();",
      "server.listen(0, '127.0.0.1', () => fs.writeFileSync(process.argv[1], String(server.address().port)));"
    ]

withListeningDescendant :: (Async.Async ExitCode -> String -> IO () -> IO ()) -> IO ()
withListeningDescendant action = do
  portPath <- makeTempPath "wasp-isolated-child-port"
  rootExitPath <- makeTempPath "wasp-isolated-root-exit"
  Async.withAsync
    (runJob ignoreOutput $ JobProcess.run_ $ nodeWithArgs descendantRootScript [listeningServerScript, portPath, rootExitPath])
    ( \running -> do
        waitUntil "descendant listening" $ doesFileExist portPath
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
assertGroup :: ProcessGroupID -> Bool -> IO ()
assertGroup parentGroup isInteractive = do
  childGroup <- newEmptyMVar
  let process = node "console.log(process.pid); setInterval(() => {}, 1000);"
      captureGroup _ _ text = case readMaybe $ T.unpack text of
        Nothing -> fail $ "Invalid child PID: " <> T.unpack text
        Just pid -> Posix.getProcessGroupIDOf pid >>= putMVar childGroup
      action = JobProcess.run_ $ if isInteractive then JobProcess.interactive process else process
  Async.withAsync (runJob captureGroup action) $ \running -> do
    group <- timeout 5000000 $ takeMVar childGroup
    if isInteractive
      then group `shouldBe` Just parentGroup
      else do
        isJust group `shouldBe` True
        (group == Just parentGroup) `shouldBe` False
    Async.cancel running
#endif
