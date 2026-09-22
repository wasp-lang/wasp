{-# LANGUAGE CPP #-}

module ProcessTest where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Monad (when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Process.Util (isPortAvailable, makeTempPath, waitUntil)
import qualified Wasp.Process as Process
#if !mingw32_HOST_OS
import qualified System.Posix.Process as Posix
import System.Posix.Types (ProcessGroupID)
#endif

spec_process :: Spec
spec_process = describe "Process.run" $ do
  it "drains all output before returning the exit code" $ do
    output <- newIORef T.empty
    let emit _ text = modifyIORef' output (<> text)
    Process.run Process.NoInput (node "process.stdout.write('x'.repeat(200000)); process.exitCode = 7;") emit
      `shouldReturn` ExitFailure 7
    readIORef output `shouldReturn` T.replicate 200000 "x"

  it "stops descendants on cancellation, including after the root exits" $ do
    mapM_ assertDescendantCleanup [False, True]

  it "forces a command that ignores graceful interruption to stop" $ do
    ready <- newEmptyMVar
    let script = "process.on('SIGINT', () => {}); process.on('SIGTERM', () => {}); console.log('ready'); setInterval(() => {}, 1000);"
    Async.withAsync (Process.run Process.NoInput (node script) (\_ _ -> putMVar ready ())) $ \running -> do
      timeout 5000000 (takeMVar ready) `shouldReturn` Just ()
      timeout 7000000 (Async.cancel running) `shouldReturn` Just ()

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
node script = P.proc "node" ["-e", script]

assertDescendantCleanup :: Bool -> IO ()
assertDescendantCleanup rootExits = do
  portPath <- makeTempPath "wasp-isolated-child-port"
  let child :: String
      child = "const fs = require('node:fs'); const server = require('node:net').createServer(); server.listen(0, '127.0.0.1', () => fs.writeFileSync(process.argv[1], String(server.address().port)));"
  let script =
        "require('node:child_process').spawn(process.execPath, ['-e', "
          <> show child
          <> ", "
          <> show portPath
          <> "], {stdio: 'inherit'});"
          <> if rootExits then "setTimeout(() => process.exit(0), 200);" else "setInterval(() => {}, 1000);"
  Async.withAsync
    (Process.run Process.NoInput (node script) (\_ _ -> return ()))
    ( \running -> do
        waitUntil "descendant listening" $ doesFileExist portPath
        port <- readFile portPath
        isPortAvailable port `shouldReturn` False
        when rootExits $ threadDelay 400000
        timeout 7000000 (Async.cancel running) `shouldReturn` Just ()
        isPortAvailable port `shouldReturn` True
    )
    `finally` (doesFileExist portPath >>= \exists -> when exists $ removeFile portPath)

#if !mingw32_HOST_OS
assertGroup :: ProcessGroupID -> Process.InputMode -> IO ()
assertGroup parentGroup inputMode = do
  output <- newIORef T.empty
  let process = P.proc "sh" ["-c", "ps -o pgid= -p $$"]
  Process.run inputMode process (\_ text -> modifyIORef' output (<> text)) `shouldReturn` ExitSuccess
  group <- read . T.unpack <$> readIORef output
  (group == parentGroup) `shouldBe` (inputMode == Process.InheritTerminal)
#endif
