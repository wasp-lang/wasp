module Generator.ServerGenerator.StartTest where

import Control.Concurrent (newChan, threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Exception (SomeException, finally, try)
import Control.Monad (void, when)
import Job.Process.LongRunningTest (isPortAvailable, isProcessAlive, killProcess, makeTempPath, trim, waitUntil)
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (readFile')
import System.Info (os)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldNotBe, shouldReturn)
import qualified Wasp.Generator.ServerGenerator.Common as ServerGenerator.Common
import Wasp.Generator.ServerGenerator.Start
  ( ServerEffect (..),
    ServerProcessController,
    newServerProcessController,
    notifyFailedCompile,
    notifySuccessfulCompile,
    startServer,
  )
import Wasp.Util (secondsToMicroSeconds)

spec_ServerEffect :: Spec
spec_ServerEffect =
  describe "ServerEffect" $
    it "combines effects by choosing the strongest" $
      [ mconcat [],
        NoServerEffect <> RestartServer,
        RestartServer <> NoServerEffect,
        RestartServer <> RestartServer,
        RestartServer <> RebundleAndRestartServer,
        RebundleAndRestartServer <> RestartServer
      ]
        `shouldBe` [ NoServerEffect,
                     RestartServer,
                     RestartServer,
                     RestartServer,
                     RebundleAndRestartServer,
                     RebundleAndRestartServer
                   ]

spec_ServerProcessController :: Spec
spec_ServerProcessController =
  describe "server process controller" $ do
    it "starts, restarts, and stops the server across compile outcomes" $
      withGeneratedAppDirFixture $ \fixture -> do
        chan <- newChan
        controller <- newServerProcessController
        generatedAppDir <- SP.parseAbsDir $ _generatedAppDirPath fixture
        withAsync (startServer generatedAppDir controller chan) $ \controllerJob -> do
          waitForServerStart fixture
          initialPid <- readServerPid fixture
          serverPort <- readServerPort fixture
          readBundleCount fixture `shouldReturn` 1
          isPortAvailable serverPort `shouldReturn` False

          -- Client-only change: no bundle, no restart.
          notifySuccessfulCompileOrFail controller NoServerEffect
          readBundleCount fixture `shouldReturn` 1
          readServerPid fixture `shouldReturn` initialPid
          isProcessAlive initialPid `shouldReturn` True

          -- Runtime-only change: restart without bundling.
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller RestartServer
          waitForServerStart fixture
          restartOnlyPid <- readServerPid fixture
          restartOnlyPid `shouldNotBe` initialPid
          isProcessAlive initialPid `shouldReturn` False
          readBundleCount fixture `shouldReturn` 1

          -- Server source change: bundle + restart.
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller RebundleAndRestartServer
          waitForServerStart fixture
          restartedPid <- readServerPid fixture
          restartedPid `shouldNotBe` restartOnlyPid
          isProcessAlive restartOnlyPid `shouldReturn` False
          readBundleCount fixture `shouldReturn` 2

          -- Stale exit notification from the stopped process must not
          -- trigger a restart of its replacement.
          threadDelay $ secondsToMicroSeconds 0.5
          notifySuccessfulCompileOrFail controller NoServerEffect
          readBundleCount fixture `shouldReturn` 2
          readServerPid fixture `shouldReturn` restartedPid
          isProcessAlive restartedPid `shouldReturn` True

          -- Failed compile stops the server and releases its port before returning.
          notifyFailedCompileOrFail controller
          isProcessAlive restartedPid `shouldReturn` False
          isPortAvailable serverPort `shouldReturn` True

          -- Next successful compile brings the server back even without
          -- server-related changes.
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller NoServerEffect
          waitForServerStart fixture
          recoveredPid <- readServerPid fixture
          readBundleCount fixture `shouldReturn` 3

          -- Restart-only also rebundles when there is no known-good server.
          notifyFailedCompileOrFail controller
          isProcessAlive recoveredPid `shouldReturn` False
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller RestartServer
          waitForServerStart fixture
          restartRecoveredPid <- readServerPid fixture
          readBundleCount fixture `shouldReturn` 4

          -- Cancelling the controller job stops the server and releases its port.
          cancel controllerJob
          isProcessAlive restartRecoveredPid `shouldReturn` False
          isPortAvailable serverPort `shouldReturn` True
          clearServerPid fixture

    -- TODO: Windows Job Objects need separate root/tree handles; native supervisor work is out of scope here.
    when (os /= "mingw32") $
      it "cleans up a crashed server with a pipe-holding child before restarting it" $
        withGeneratedAppDirFixture $ \fixture -> do
          writeServerStartScript fixture crashingServerScript
          chan <- newChan
          controller <- newServerProcessController
          generatedAppDir <- SP.parseAbsDir $ _generatedAppDirPath fixture
          withAsync (startServer generatedAppDir controller chan) $ \controllerJob -> do
            waitUntil "crashed server pid file" $ doesFileExist $ serverPidFilePath fixture
            crashedPid <- readServerPid fixture
            waitUntil "leftover process port file" $ doesFileExist $ leftoverPortFilePath fixture
            leftoverPort <- trim <$> readFile' (leftoverPortFilePath fixture)
            waitUntil "server crash" $ not <$> isProcessAlive crashedPid
            waitUntil "leftover process port release" $ isPortAvailable leftoverPort
            mapM_ removeFileIfExists [leftoverPidFilePath fixture, leftoverPortFilePath fixture]

            writeServerStartScript fixture loopingServerScript
            clearServerPid fixture
            notifySuccessfulCompileOrFail controller NoServerEffect
            waitForServerStart fixture
            newPid <- readServerPid fixture
            newPid `shouldNotBe` crashedPid
            readBundleCount fixture `shouldReturn` 2
            newPort <- readServerPort fixture

            cancel controllerJob
            isPortAvailable newPort `shouldReturn` True
            clearServerPid fixture

newtype GeneratedAppDirFixture = GeneratedAppDirFixture
  { _generatedAppDirPath :: FilePath
  }

withGeneratedAppDirFixture :: (GeneratedAppDirFixture -> IO ()) -> IO ()
withGeneratedAppDirFixture test = do
  generatedAppDirPath <- makeTempPath "wasp-server-controller-test"
  let fixture = GeneratedAppDirFixture generatedAppDirPath
  createDirectoryIfMissing True $ serverBundleDirPath fixture
  createDirectoryIfMissing True $ dotenvModuleDirPath fixture
  writeFile (serverDirPath fixture </> "package.json") packageJson
  writeFile (serverDirPath fixture </> "bundle.js") bundleScript
  writeFile (dotenvModuleDirPath fixture </> "config.js") ""
  writeFile (serverDirPath fixture </> serverPortFileName) "0"
  writeServerStartScript fixture loopingServerScript
  test fixture `finally` cleanUpFixture fixture
  where
    packageJson =
      unlines
        [ "{",
          "  \"name\": \"wasp-server-controller-test\",",
          "  \"version\": \"1.0.0\",",
          "  \"scripts\": {",
          "    \"bundle\": \"node bundle.js\",",
          "    \"start\": \"node bundle/server.js\"",
          "  }",
          "}"
        ]

    bundleScript = "require('node:fs').appendFileSync('bundles.log', 'bundled\\n');\n"

cleanUpFixture :: GeneratedAppDirFixture -> IO ()
cleanUpFixture fixture = do
  mapM_ killPidFromFile [serverPidFilePath fixture, leftoverPidFilePath fixture]
  void (try (removeDirectoryRecursive $ _generatedAppDirPath fixture) :: IO (Either SomeException ()))
  where
    killPidFromFile pidFilePath = do
      exists <- doesFileExist pidFilePath
      when exists $ readFile' pidFilePath >>= killProcess

loopingServerScript :: String
loopingServerScript =
  unlines
    [ "const fs = require('node:fs');",
      "const net = require('node:net');",
      "const port = Number(fs.readFileSync('" <> serverPortFileName <> "', 'utf8'));",
      "const server = net.createServer();",
      "server.listen(port, '127.0.0.1', () => {",
      "  fs.writeFileSync('" <> serverPortFileName <> "', String(server.address().port));",
      "  fs.writeFileSync('server.pid', String(process.pid));",
      "});",
      "const stop = () => server.close(() => process.exit(0));",
      "process.on('SIGINT', stop);",
      "process.on('SIGTERM', stop);"
    ]

crashingServerScript :: String
crashingServerScript =
  unlines
    [ "const { spawn } = require('node:child_process');",
      "const fs = require('node:fs');",
      "const childScript = \"const net = require('node:net'); const server = net.createServer(); server.listen(0, '127.0.0.1', () => process.send(String(server.address().port)));\";",
      "const child = spawn(process.execPath, ['-e', childScript], { stdio: ['ignore', 'inherit', 'inherit', 'ipc'] });",
      "child.once('message', (port) => {",
      "  fs.writeFileSync('server.pid', String(process.pid));",
      "  fs.writeFileSync('leftover.pid', String(child.pid));",
      "  fs.writeFileSync('leftover-port.txt', String(port));",
      "  process.exit(1);",
      "});"
    ]

writeServerStartScript :: GeneratedAppDirFixture -> String -> IO ()
writeServerStartScript fixture = writeFile (serverBundleDirPath fixture </> "server.js")

serverDirPath :: GeneratedAppDirFixture -> FilePath
serverDirPath fixture =
  _generatedAppDirPath fixture </> SP.fromRelDir ServerGenerator.Common.serverRootDirInGeneratedAppDir

serverBundleDirPath :: GeneratedAppDirFixture -> FilePath
serverBundleDirPath fixture = serverDirPath fixture </> "bundle"

dotenvModuleDirPath :: GeneratedAppDirFixture -> FilePath
dotenvModuleDirPath fixture = serverDirPath fixture </> "node_modules" </> "dotenv"

serverPidFilePath :: GeneratedAppDirFixture -> FilePath
serverPidFilePath fixture = serverDirPath fixture </> "server.pid"

leftoverPidFilePath :: GeneratedAppDirFixture -> FilePath
leftoverPidFilePath fixture = serverDirPath fixture </> "leftover.pid"

leftoverPortFilePath :: GeneratedAppDirFixture -> FilePath
leftoverPortFilePath fixture = serverDirPath fixture </> "leftover-port.txt"

bundlesLogFilePath :: GeneratedAppDirFixture -> FilePath
bundlesLogFilePath fixture = serverDirPath fixture </> "bundles.log"

serverPortFileName :: FilePath
serverPortFileName = "server-port.txt"

waitForServerStart :: GeneratedAppDirFixture -> IO ()
waitForServerStart fixture =
  waitUntilWithin "server start" 30 $ do
    pidFileExists <- doesFileExist $ serverPidFilePath fixture
    if pidFileExists
      then do
        serverPid <- readServerPid fixture
        serverPort <- readServerPort fixture
        processAlive <- isProcessAlive serverPid
        portAvailable <- isPortAvailable serverPort
        return $ processAlive && not portAvailable
      else return False

readServerPid :: GeneratedAppDirFixture -> IO String
readServerPid fixture = trim <$> readFile' (serverPidFilePath fixture)

readServerPort :: GeneratedAppDirFixture -> IO String
readServerPort fixture = trim <$> readFile' (serverDirPath fixture </> serverPortFileName)

clearServerPid :: GeneratedAppDirFixture -> IO ()
clearServerPid fixture = do
  removeFileIfExists $ serverPidFilePath fixture

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath

readBundleCount :: GeneratedAppDirFixture -> IO Int
readBundleCount fixture = do
  exists <- doesFileExist $ bundlesLogFilePath fixture
  if exists
    then length . lines <$> readFile' (bundlesLogFilePath fixture)
    else return 0

notifySuccessfulCompileOrFail :: ServerProcessController -> ServerEffect -> IO ()
notifySuccessfulCompileOrFail controller serverEffect =
  failUnlessHandledInTime $ notifySuccessfulCompile controller serverEffect

notifyFailedCompileOrFail :: ServerProcessController -> IO ()
notifyFailedCompileOrFail = failUnlessHandledInTime . notifyFailedCompile

failUnlessHandledInTime :: IO () -> IO ()
failUnlessHandledInTime notify =
  timeout (secondsToMicroSeconds 60) notify
    >>= maybe (expectationFailure "Controller did not handle the notification in time") return

waitUntilWithin :: String -> Double -> IO Bool -> IO ()
waitUntilWithin label seconds condition = go attempts
  where
    attempts = ceiling $ seconds / 0.25 :: Int
    go remainingAttempts
      | remainingAttempts <= 0 = expectationFailure $ "Timed out waiting for " <> label
      | otherwise = do
          result <- condition
          if result
            then return ()
            else do
              threadDelay (secondsToMicroSeconds 0.25)
              go $ remainingAttempts - 1
