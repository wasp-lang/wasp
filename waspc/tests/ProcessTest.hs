{-# LANGUAGE CPP #-}

module ProcessTest where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally, fromException)
import Control.Monad (when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.Info (os)
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Process.Util (isPortAvailable, isProcessAlive, killProcess, makeTempPath, readProcessId, waitUntil)
import qualified Wasp.Process as Process
#if !mingw32_HOST_OS
import qualified System.Posix.Process as Posix
import System.Posix.Types (ProcessGroupID)
import Text.Read (readMaybe)
#endif

spec_process :: Spec
spec_process = describe "Process.run" $ do
  it "drains all output before returning the exit code" $ do
    output <- newIORef T.empty
    let emit _ text = modifyIORef' output (<> text)
    Process.run Process.NoInput (node "process.stdout.write('x'.repeat(200000)); process.exitCode = 7;") emit
      `shouldReturn` ExitFailure 7
    readIORef output `shouldReturn` T.replicate 200000 "x"

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
      (Process.run Process.NoInput (node script) (\_ _ -> return ()))
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

  it "does not stop another isolated command when one is cancelled" $ do
    firstReady <- newEmptyMVar
    secondReady <- newEmptyMVar
    let script = node "console.log('ready'); setInterval(() => {}, 1000);"
    Async.withAsync (Process.run Process.NoInput script (\_ _ -> putMVar firstReady ())) $ \first ->
      Async.withAsync (Process.run Process.NoInput script (\_ _ -> putMVar secondReady ())) $ \second -> do
        timeout 5000000 (takeMVar firstReady) `shouldReturn` Just ()
        timeout 5000000 (takeMVar secondReady) `shouldReturn` Just ()
        Async.cancel first
        Async.poll second >>= \case
          Nothing -> return ()
          Just _ -> fail "Cancelling one command stopped another command"

#if !mingw32_HOST_OS
  it "keeps terminal commands in the caller's group and isolates NoInput commands" $ do
    parentGroup <- Posix.getProcessGroupID
    mapM_ (assertGroup parentGroup) [Process.InheritTerminal, Process.NoInput]
#endif

node :: String -> P.CreateProcess
node script = nodeWithArgs script []

nodeWithArgs :: String -> [String] -> P.CreateProcess
nodeWithArgs script args = P.proc "node" $ ["-e", script] <> args

withListeningDescendant :: (Async.Async ExitCode -> String -> IO () -> IO ()) -> IO ()
withListeningDescendant action = do
  portPath <- makeTempPath "wasp-isolated-child-port"
  rootExitPath <- makeTempPath "wasp-isolated-root-exit"
  Async.withAsync
    (Process.run Process.NoInput (nodeWithArgs descendantRootScript [listeningServerScript, portPath, rootExitPath]) (\_ _ -> return ()))
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
assertGroup :: ProcessGroupID -> Process.InputMode -> IO ()
assertGroup parentGroup inputMode = do
  childGroup <- newEmptyMVar
  let process = node "console.log(process.pid); setInterval(() => {}, 1000);"
      captureGroup _ text = case readMaybe $ T.unpack text of
        Nothing -> fail $ "Invalid child PID: " <> T.unpack text
        Just pid -> Posix.getProcessGroupIDOf pid >>= putMVar childGroup
  Async.withAsync (Process.run inputMode process captureGroup) $ \running -> do
    group <- timeout 5000000 $ takeMVar childGroup
    case inputMode of
      Process.InheritTerminal -> group `shouldBe` Just parentGroup
      Process.NoInput -> do
        isJust group `shouldBe` True
        (group == Just parentGroup) `shouldBe` False
    Async.cancel running
#endif
