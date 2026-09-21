module Job.Process.ManagedTest where

import Control.Concurrent (Chan, newChan, readChan)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.Info (os)
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import Test.Process.Util (isPortAvailable, makeTempPath, waitUntil)
import qualified Wasp.Job as J
import qualified Wasp.Job.Kind as Kind
import qualified Wasp.Job.Output.Event as Event
import qualified Wasp.Job.Process as JobProcess
import Wasp.Util (secondsToMicroSeconds)

spec_managedSubprocess :: Spec
spec_managedSubprocess =
  describe "managed Subprocess" $ do
    it "stops an owned process tree when a Job is cancelled" $ do
      portFilePath <- makeTempPath "wasp-long-running-job-port"
      chan <- newChan
      let jobAction = do
            subprocess <- JobProcess.spawn (nodeScript $ portOwningChildProcessScript portFilePath)
            liftIO (JobProcess.wait subprocess) >>= J.requireExitSuccess
      let job = J.runJob Kind.WebApp jobAction chan
      Async.withAsync
        job
        ( \jobAsync -> do
            waitUntil "job child port file" $ doesFileExist portFilePath
            port <- readFile portFilePath
            isPortAvailable port `shouldReturn` False

            Async.cancel jobAsync

            isPortAvailable port `shouldReturn` True
        )
        `finally` removeFileIfExists portFilePath

    -- TODO: Windows Job Objects need separate root/tree handles; native supervisor work is out of scope here.
    when (os /= "mingw32") $
      it "kills process-group descendants after the root process exits" $ do
        portFilePath <- makeTempPath "wasp-long-running-child-port"
        let action = do
              subprocess <- JobProcess.spawn (nodeScript $ exitingRootWithPortOwningChildScript portFilePath)
              port <- liftIO $ do
                waitUntil "child port file" $ doesFileExist portFilePath
                port <- readFile portFilePath
                maybeRootExit <- timeout (secondsToMicroSeconds 5) $ JobProcess.wait subprocess
                maybeRootExit `shouldBe` Just ExitSuccess
                JobProcess.poll subprocess `shouldReturn` Just ExitSuccess
                isPortAvailable port `shouldReturn` False
                return port
              startedAt <- liftIO getCurrentTime
              JobProcess.stop subprocess
              stoppedAt <- liftIO getCurrentTime
              liftIO $ do
                realToFrac (stoppedAt `diffUTCTime` startedAt) `shouldSatisfy` (< maxAcceptableStopSeconds)
                isPortAvailable port `shouldReturn` True
        void (runJob action) `finally` removeFileIfExists portFilePath

    when (os /= "mingw32") $
      it "interrupts the process so it can exit gracefully before being killed" $ do
        startedFilePath <- makeTempPath "wasp-long-running-started"
        gracefulExitFilePath <- makeTempPath "wasp-long-running-graceful-exit"
        let action = do
              subprocess <- JobProcess.spawn (nodeScript $ gracefulProcessScript startedFilePath gracefulExitFilePath)
              liftIO $ waitUntil "process start" $ doesFileExist startedFilePath
              JobProcess.stop subprocess
              liftIO $ waitUntil "graceful exit marker" $ doesFileExist gracefulExitFilePath
        void (runJob action)
          `finally` mapM_ removeFileIfExists [startedFilePath, gracefulExitFilePath]

    it "kills a process that ignores graceful stop signals" $ do
      startedFilePath <- makeTempPath "wasp-long-running-stubborn"
      let action = do
            subprocess <- JobProcess.spawn (nodeScript $ stubbornProcessScript startedFilePath)
            liftIO $ waitUntil "process start" $ doesFileExist startedFilePath
            startedAt <- liftIO getCurrentTime
            JobProcess.stop subprocess
            stoppedAt <- liftIO getCurrentTime
            liftIO $ do
              realToFrac (stoppedAt `diffUTCTime` startedAt) `shouldSatisfy` (< maxAcceptableStopSeconds)
              maybeRootExit <- timeout (secondsToMicroSeconds 5) $ JobProcess.wait subprocess
              maybeRootExit `shouldSatisfy` isJust
      void (runJob action) `finally` removeFileIfExists startedFilePath

    it "releases a descendant-owned port before stop returns" $ do
      portFilePath <- makeTempPath "wasp-long-running-port"
      let action = do
            subprocess <- JobProcess.spawn (nodeScript $ portOwningChildProcessScript portFilePath)
            port <- liftIO $ do
              waitUntil "child-owned port" $ doesFileExist portFilePath
              port <- readFile portFilePath
              isPortAvailable port `shouldReturn` False
              return port

            JobProcess.stop subprocess

            liftIO $ isPortAvailable port `shouldReturn` True
      void (runJob action) `finally` removeFileIfExists portFilePath

    it "decodes chunk-split and incomplete UTF-8 output" $ do
      let euroSignCount = 40000 :: Int
      let expectedOutput = T.replicate euroSignCount "€" <> "�"
      let script =
            "process.stdout.write(Buffer.concat([Buffer.from('€'.repeat("
              <> show euroSignCount
              <> ")), Buffer.from([0xe2])]));"
      chan <- runJob $ do
        subprocess <- JobProcess.spawn (nodeScript script)
        maybeExitCode <- liftIO $ timeout (secondsToMicroSeconds 20) $ JobProcess.wait subprocess
        case maybeExitCode of
          Nothing -> do
            JobProcess.stop subprocess
            liftIO $ expectationFailure "Timed out waiting for process exit; output forwarding likely stalled"
          Just exitCode -> do
            liftIO $ exitCode `shouldBe` ExitSuccess
            JobProcess.stop subprocess
      output <- collectQueuedOutput chan
      output `shouldBe` expectedOutput

runJob :: J.Job () -> IO (Chan Event.JobEvent)
runJob action = do
  chan <- newChan
  exitCode <- J.runJob Kind.Server action chan
  exitCode `shouldBe` ExitSuccess
  return chan

-- Covers graceful stop, hard-stop escalation, and polling slack.
maxAcceptableStopSeconds :: Double
maxAcceptableStopSeconds = 2

nodeScript :: String -> P.CreateProcess
nodeScript script = P.proc "node" ["-e", script]

exitingRootWithPortOwningChildScript :: FilePath -> String
exitingRootWithPortOwningChildScript portFilePath =
  unlines
    [ "const { spawn } = require('node:child_process');",
      "const childScript = " <> jsString portOwningChildScript <> ";",
      "spawn(process.execPath, ['-e', childScript, " <> jsString portFilePath <> "], { stdio: 'inherit' });",
      "setTimeout(() => process.exit(0), 200);"
    ]

gracefulProcessScript :: FilePath -> FilePath -> String
gracefulProcessScript startedFilePath gracefulExitFilePath =
  unlines
    [ "const fs = require('node:fs');",
      "fs.writeFileSync(" <> jsString startedFilePath <> ", 'started');",
      "process.on('SIGINT', () => {",
      "  fs.writeFileSync(" <> jsString gracefulExitFilePath <> ", 'done');",
      "  process.exit(0);",
      "});",
      "setInterval(() => {}, 1000);"
    ]

stubbornProcessScript :: FilePath -> String
stubbornProcessScript startedFilePath =
  unlines
    [ "const fs = require('node:fs');",
      "fs.writeFileSync(" <> jsString startedFilePath <> ", 'started');",
      "process.on('SIGINT', () => {});",
      "process.on('SIGTERM', () => {});",
      "setInterval(() => {}, 1000);"
    ]

portOwningChildProcessScript :: FilePath -> String
portOwningChildProcessScript portFilePath =
  unlines
    [ "const { spawn } = require('node:child_process');",
      "const childScript = " <> jsString portOwningChildScript <> ";",
      "spawn(process.execPath, ['-e', childScript, " <> jsString portFilePath <> "], { stdio: 'inherit' });",
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

collectQueuedOutput :: Chan Event.JobEvent -> IO T.Text
collectQueuedOutput chan = go []
  where
    go collected = do
      maybeMessage <- timeout (secondsToMicroSeconds 0.2) $ readChan chan
      case maybeMessage of
        Nothing -> return $ T.concat $ reverse collected
        Just Event.JobEvent {Event._eventData = Event.JobOutput _ output} -> go (output : collected)
        Just _ -> go collected

jsString :: String -> String
jsString = show

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath
