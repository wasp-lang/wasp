module Job.Process.LongRunningTest where

import Control.Concurrent (Chan, newChan, readChan)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Monad (void, when)
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
import qualified Wasp.Job.Process.LongRunning as LongRunning
import Wasp.Util (secondsToMicroSeconds)

spec_LongRunningProcess :: Spec
spec_LongRunningProcess =
  describe "LongRunningProcess" $ do
    it "stops an owned process tree when a Job is cancelled" $ do
      portFilePath <- makeTempPath "wasp-long-running-job-port"
      chan <- newChan
      let job = LongRunning.runAsJob (nodeScript $ portOwningChildProcessScript portFilePath) J.WebApp chan
      ( Async.withAsync job $ \jobAsync -> do
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
        void $ withJobOutputSink $ \outputSink -> do
          longRunningProcess <- LongRunning.start (nodeScript $ exitingRootWithPortOwningChildScript portFilePath) outputSink
          let cleanup = LongRunning.stop longRunningProcess >> removeFileIfExists portFilePath
          ( do
              waitUntil "child port file" $ doesFileExist portFilePath
              port <- readFile portFilePath
              maybeRootExit <- timeout (secondsToMicroSeconds 5) $ LongRunning.waitForRootExit longRunningProcess
              maybeRootExit `shouldBe` Just ExitSuccess
              LongRunning.pollRootExit longRunningProcess `shouldReturn` Just ExitSuccess
              isPortAvailable port `shouldReturn` False

              startedAt <- getCurrentTime
              LongRunning.stop longRunningProcess
              stoppedAt <- getCurrentTime

              realToFrac (stoppedAt `diffUTCTime` startedAt) `shouldSatisfy` (< maxAcceptableStopSeconds)
              isPortAvailable port `shouldReturn` True
            )
            `finally` cleanup

    when (os /= "mingw32") $
      it "interrupts the process so it can exit gracefully before being killed" $ do
        startedFilePath <- makeTempPath "wasp-long-running-started"
        gracefulExitFilePath <- makeTempPath "wasp-long-running-graceful-exit"
        void $ withJobOutputSink $ \outputSink -> do
          longRunningProcess <- LongRunning.start (nodeScript $ gracefulProcessScript startedFilePath gracefulExitFilePath) outputSink
          let cleanup =
                LongRunning.stop longRunningProcess
                  >> mapM_ removeFileIfExists [startedFilePath, gracefulExitFilePath]
          ( do
              waitUntil "process start" $ doesFileExist startedFilePath
              LongRunning.stop longRunningProcess
              waitUntil "graceful exit marker" $ doesFileExist gracefulExitFilePath
            )
            `finally` cleanup

    it "kills a process that ignores graceful stop signals" $ do
      startedFilePath <- makeTempPath "wasp-long-running-stubborn"
      void $ withJobOutputSink $ \outputSink -> do
        longRunningProcess <- LongRunning.start (nodeScript $ stubbornProcessScript startedFilePath) outputSink
        let cleanup = LongRunning.stop longRunningProcess >> removeFileIfExists startedFilePath
        ( do
            waitUntil "process start" $ doesFileExist startedFilePath
            startedAt <- getCurrentTime
            LongRunning.stop longRunningProcess
            stoppedAt <- getCurrentTime
            realToFrac (stoppedAt `diffUTCTime` startedAt) `shouldSatisfy` (< maxAcceptableStopSeconds)
            maybeRootExit <- timeout (secondsToMicroSeconds 5) $ LongRunning.waitForRootExit longRunningProcess
            maybeRootExit `shouldSatisfy` isJust
          )
          `finally` cleanup

    it "releases a descendant-owned port before stop returns" $ do
      portFilePath <- makeTempPath "wasp-long-running-port"
      void $ withJobOutputSink $ \outputSink -> do
        longRunningProcess <- LongRunning.start (nodeScript $ portOwningChildProcessScript portFilePath) outputSink
        let cleanup = LongRunning.stop longRunningProcess >> removeFileIfExists portFilePath
        ( do
            waitUntil "child-owned port" $ doesFileExist portFilePath
            port <- readFile portFilePath
            isPortAvailable port `shouldReturn` False

            LongRunning.stop longRunningProcess

            isPortAvailable port `shouldReturn` True
          )
          `finally` cleanup

    it "decodes chunk-split and incomplete UTF-8 output" $ do
      let euroSignCount = 40000 :: Int
      let expectedOutput = T.replicate euroSignCount "€" <> "�"
      let script =
            "process.stdout.write(Buffer.concat([Buffer.from('€'.repeat("
              <> show euroSignCount
              <> ")), Buffer.from([0xe2])]));"
      chan <- withJobOutputSink $ \outputSink -> do
        longRunningProcess <- LongRunning.start (nodeScript script) outputSink
        maybeExitCode <- timeout (secondsToMicroSeconds 20) $ LongRunning.waitForRootExit longRunningProcess
        case maybeExitCode of
          Nothing -> do
            LongRunning.stop longRunningProcess
            expectationFailure "Timed out waiting for process exit; output forwarding likely stalled"
          Just exitCode -> do
            exitCode `shouldBe` ExitSuccess
            LongRunning.stop longRunningProcess
      output <- collectQueuedOutput chan
      output `shouldBe` expectedOutput

withJobOutputSink :: (J.JobOutputSink -> IO ()) -> IO (Chan J.JobMessage)
withJobOutputSink action = do
  chan <- newChan
  exitCode <- J.makeJob J.Server (\outputSink -> action outputSink >> return ExitSuccess) chan
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

collectQueuedOutput :: Chan J.JobMessage -> IO T.Text
collectQueuedOutput chan = go []
  where
    go collected = do
      maybeMessage <- timeout (secondsToMicroSeconds 0.2) $ readChan chan
      case maybeMessage of
        Nothing -> return $ T.concat $ reverse collected
        Just J.JobMessage {J._data = J.JobOutput output _} -> go (output : collected)
        Just _ -> go collected

jsString :: String -> String
jsString = show

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath
