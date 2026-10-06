{-# LANGUAGE CPP #-}

module Job.ProcessTest where

import Control.Concurrent (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally, fromException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.Info (os)
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Process.Util (isPortAvailable, isProcessAlive, killProcess, makeTempPath, readProcessId, waitUntil)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output as Output
import qualified Wasp.Job.Process as JobProcess
import Wasp.Util (secondsToMicroSeconds)
#if !mingw32_HOST_OS
import Data.Maybe (isJust)
import qualified System.Posix.Process as Posix
import System.Posix.Types (ProcessGroupID)
import Text.Read (readMaybe)
#endif

spec_JobProcess :: Spec
spec_JobProcess = do
  describe "JobProcess.run_" $ do
    it "fails the job on a nonzero child exit" $ do
      Job.runJob ignoreOutput (JobProcess.run_ $ node "process.exit(7)")
        `shouldReturn` ExitFailure 7

  describe "JobProcess.run" $ do
    it "returns a nonzero child exit for explicit handling" $ do
      let action = do
            exitCode <- JobProcess.run $ node "process.exit(7)"
            liftIO $ exitCode `shouldBe` ExitFailure 7
      Job.runJob ignoreOutput action `shouldReturn` ExitSuccess

    it "forwards stdout and stderr to the job's sink" $ do
      chunks <- newMVar []
      let sink stream output = modifyMVar_ chunks $ return . ((stream, output) :)
          action = JobProcess.run_ $ node "process.stdout.write('out'); process.stderr.write('err');"
      Job.runJob sink action `shouldReturn` ExitSuccess
      sort <$> readMVar chunks `shouldReturn` [(Job.Stdout, "out"), (Job.Stderr, "err")]

    it "forwards all output before returning the exit code" $ do
      let action = JobProcess.run_ $ node "process.stdout.write('x'.repeat(200000)); process.exitCode = 7;"
      Output.capturing (`Job.runJob` action)
        `shouldReturn` (ExitFailure 7, T.replicate 200000 "x")

    it "gives commands an empty stdin" $ do
      let action = JobProcess.run_ $ node "process.stdin.resume(); process.stdin.on('end', () => process.exit(3));"
      timeout (secondsToMicroSeconds 10) (Job.runJob ignoreOutput action)
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
        (Job.runJob ignoreOutput $ JobProcess.run_ $ node script)
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
      Async.withAsync (Job.runJob (\_ _ -> putMVar firstReady ()) action) $ \first ->
        Async.withAsync (Job.runJob (\_ _ -> putMVar secondReady ()) action) $ \second -> do
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

ignoreOutput :: Job.Sink
ignoreOutput _ _ = return ()

withListeningDescendant :: (Async.Async ExitCode -> String -> IO () -> IO ()) -> IO ()
withListeningDescendant action = do
  portPath <- makeTempPath "wasp-isolated-child-port"
  rootExitPath <- makeTempPath "wasp-isolated-root-exit"
  Async.withAsync
    (Job.runJob ignoreOutput $ JobProcess.run_ $ nodeWithArgs descendantRootScript [listeningServerScript, portPath, rootExitPath])
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
      captureGroup _ text = case readMaybe $ T.unpack text of
        Nothing -> fail $ "Invalid child PID: " <> T.unpack text
        Just pid -> Posix.getProcessGroupIDOf pid >>= putMVar childGroup
      action = JobProcess.run_ $ if isInteractive then JobProcess.interactive process else process
  Async.withAsync (Job.runJob captureGroup action) $ \running -> do
    group <- timeout 5000000 $ takeMVar childGroup
    if isInteractive
      then group `shouldBe` Just parentGroup
      else do
        isJust group `shouldBe` True
        (group == Just parentGroup) `shouldBe` False
    Async.cancel running
#endif
