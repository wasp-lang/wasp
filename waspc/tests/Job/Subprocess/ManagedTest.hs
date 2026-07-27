module Job.Subprocess.ManagedTest where

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
import qualified Wasp.Job.Subprocess as Subprocess
import Wasp.Util (secondsToMicroSeconds)

spec_managedSubprocess :: Spec
spec_managedSubprocess =
  describe "managed Subprocess" $ do
    it "stops an owned process tree when a Job is cancelled" $ do
      portFilePath <- makeTempPath "wasp-long-running-job-port"
      chan <- newChan
      let jobAction = do
            subprocess <- Subprocess.spawn (nodeScript $ portOwningChildProcessScript portFilePath)
            liftIO (Subprocess.wait subprocess) >>= J.requireExitSuccess
      let job = J.runJob (J.makeJob J.WebApp jobAction) chan
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
              subprocess <- Subprocess.spawn (nodeScript $ exitingRootWithPortOwningChildScript portFilePath)
              port <- liftIO $ do
                waitUntil "child port file" $ doesFileExist portFilePath
                port <- readFile portFilePath
                maybeRootExit <- timeout (secondsToMicroSeconds 5) $ Subprocess.wait subprocess
                maybeRootExit `shouldBe` Just ExitSuccess
                Subprocess.poll subprocess `shouldReturn` Just ExitSuccess
                isPortAvailable port `shouldReturn` False
                return port
              startedAt <- liftIO getCurrentTime
              Subprocess.stop subprocess
              stoppedAt <- liftIO getCurrentTime
              liftIO $ do
                realToFrac (stoppedAt `diffUTCTime` startedAt) `shouldSatisfy` (< maxAcceptableStopSeconds)
                isPortAvailable port `shouldReturn` True
        void (runJobAction action) `finally` removeFileIfExists portFilePath

    when (os /= "mingw32") $
      it "interrupts the process so it can exit gracefully before being killed" $ do
        startedFilePath <- makeTempPath "wasp-long-running-started"
        gracefulExitFilePath <- makeTempPath "wasp-long-running-graceful-exit"
        let action = do
              subprocess <- Subprocess.spawn (nodeScript $ gracefulProcessScript startedFilePath gracefulExitFilePath)
              liftIO $ waitUntil "process start" $ doesFileExist startedFilePath
              Subprocess.stop subprocess
              liftIO $ waitUntil "graceful exit marker" $ doesFileExist gracefulExitFilePath
        void (runJobAction action)
          `finally` mapM_ removeFileIfExists [startedFilePath, gracefulExitFilePath]

    it "kills a process that ignores graceful stop signals" $ do
      startedFilePath <- makeTempPath "wasp-long-running-stubborn"
      let action = do
            subprocess <- Subprocess.spawn (nodeScript $ stubbornProcessScript startedFilePath)
            liftIO $ waitUntil "process start" $ doesFileExist startedFilePath
            startedAt <- liftIO getCurrentTime
            Subprocess.stop subprocess
            stoppedAt <- liftIO getCurrentTime
            liftIO $ do
              realToFrac (stoppedAt `diffUTCTime` startedAt) `shouldSatisfy` (< maxAcceptableStopSeconds)
              maybeRootExit <- timeout (secondsToMicroSeconds 5) $ Subprocess.wait subprocess
              maybeRootExit `shouldSatisfy` isJust
      void (runJobAction action) `finally` removeFileIfExists startedFilePath

    it "releases a descendant-owned port before stop returns" $ do
      portFilePath <- makeTempPath "wasp-long-running-port"
      let action = do
            subprocess <- Subprocess.spawn (nodeScript $ portOwningChildProcessScript portFilePath)
            port <- liftIO $ do
              waitUntil "child-owned port" $ doesFileExist portFilePath
              port <- readFile portFilePath
              isPortAvailable port `shouldReturn` False
              return port

            Subprocess.stop subprocess

            liftIO $ isPortAvailable port `shouldReturn` True
      void (runJobAction action) `finally` removeFileIfExists portFilePath

    it "decodes chunk-split and incomplete UTF-8 output" $ do
      let euroSignCount = 40000 :: Int
      let expectedOutput = T.replicate euroSignCount "€" <> "�"
      let script =
            "process.stdout.write(Buffer.concat([Buffer.from('€'.repeat("
              <> show euroSignCount
              <> ")), Buffer.from([0xe2])]));"
      chan <- runJobAction $ do
        subprocess <- Subprocess.spawn (nodeScript script)
        maybeExitCode <- liftIO $ timeout (secondsToMicroSeconds 20) $ Subprocess.wait subprocess
        case maybeExitCode of
          Nothing -> do
            Subprocess.stop subprocess
            liftIO $ expectationFailure "Timed out waiting for process exit; output forwarding likely stalled"
          Just exitCode -> do
            liftIO $ exitCode `shouldBe` ExitSuccess
            Subprocess.stop subprocess
      output <- collectQueuedOutput chan
      output `shouldBe` expectedOutput

runJobAction :: J.JobAction () -> IO (Chan J.JobEvent)
runJobAction action = do
  chan <- newChan
  exitCode <- J.runJob (J.makeJob J.Server action) chan
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

collectQueuedOutput :: Chan J.JobEvent -> IO T.Text
collectQueuedOutput chan = go []
  where
    go collected = do
      maybeMessage <- timeout (secondsToMicroSeconds 0.2) $ readChan chan
      case maybeMessage of
        Nothing -> return $ T.concat $ reverse collected
        Just J.JobEvent {J._eventData = J.JobOutput output _} -> go (output : collected)
        Just _ -> go collected

jsString :: String -> String
jsString = show

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath
