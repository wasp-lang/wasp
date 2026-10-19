module Generator.ServerGenerator.StartTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Void (Void)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, doesFileExist, exeExtension, getPermissions, removeDirectoryRecursive, removeFile, setOwnerExecutable, setPermissions)
import System.FilePath ((<.>), (</>))
import System.IO (readFile')
import System.Info (os)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldNotBe, shouldReturn)
import Test.Process.Util (ProcessId, isPortAvailable, isProcessAlive, killProcess, makeTempPath, readProcessId, trim, waitUntil)
import Wasp.AppComponentUrl (makeAppComponentUrl)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as ServerGenerator.Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import Wasp.Generator.ServerGenerator.ServerEffect (ServerEffect (..))
import Wasp.Generator.ServerGenerator.Start
  ( CompileChanges (..),
    CompileOutcome (..),
    ServerEvent (..),
    ServerProcessController,
    newServerProcessController,
    notifyServer,
    startServer,
  )
import qualified Wasp.Job as Job
import Wasp.Util (secondsToMicroSeconds)

spec_ServerProcessController :: Spec
spec_ServerProcessController =
  -- The fake `tsc` and `rollup` that bundle the server are shell scripts, which
  -- Windows can't run.
  unless (os == "mingw32") $ describe "server process controller" $ do
    it "starts, restarts, and stops the server across compile outcomes" $
      withGeneratedAppDirFixture $ \fixture -> do
        controller <- newServerProcessController
        generatedAppDir <- SP.parseAbsDir $ _generatedAppDirPath fixture
        withAsync (runController controller generatedAppDir) $ \controllerJob -> do
          waitForServerStart fixture
          initialPid <- readServerPid fixture
          serverPort <- readServerPort fixture
          readBundleCount fixture `shouldReturn` 1
          readFile' (serverDirPath fixture </> "runtime-env.txt")
            `shouldReturn` "0\nhttp://localhost:0\nhttp://localhost:3000"
          isPortAvailable serverPort `shouldReturn` False

          -- Client-only change: no bundle, no restart.
          notifySuccessfulCompileOrFail controller NoChange
          readBundleCount fixture `shouldReturn` 1
          readServerPid fixture `shouldReturn` initialPid
          isProcessAlive initialPid `shouldReturn` True

          -- Runtime-only change: restart without bundling.
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller RestartServer
          waitForServerStart fixture
          restartOnlyPid <- readServerPid fixture
          restartOnlyPid `shouldNotBe` initialPid
          waitUntil "the server exits" $ not <$> isProcessAlive initialPid
          readBundleCount fixture `shouldReturn` 1

          -- Server source change: bundle + restart.
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller RebundleAndRestartServer
          waitForServerStart fixture
          restartedPid <- readServerPid fixture
          restartedPid `shouldNotBe` restartOnlyPid
          waitUntil "the server exits" $ not <$> isProcessAlive restartOnlyPid
          readBundleCount fixture `shouldReturn` 2

          -- Stale exit notification from the stopped process must not
          -- trigger a restart of its replacement.
          threadDelay $ secondsToMicroSeconds 1 `div` 2
          notifySuccessfulCompileOrFail controller NoChange
          readBundleCount fixture `shouldReturn` 2
          readServerPid fixture `shouldReturn` restartedPid
          isProcessAlive restartedPid `shouldReturn` True

          -- Failed compile stops the server and releases its port.
          notifyFailedCompileOrFail controller
          waitUntil "the server exits" $ not <$> isProcessAlive restartedPid
          waitUntil "the server port is released" $ isPortAvailable serverPort

          -- Next successful compile brings the server back even without
          -- server-related changes.
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller NoChange
          waitForServerStart fixture
          recoveredPid <- readServerPid fixture
          readBundleCount fixture `shouldReturn` 3

          -- Restart-only also rebundles when there is no known-good server.
          notifyFailedCompileOrFail controller
          waitUntil "the server exits" $ not <$> isProcessAlive recoveredPid
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller RestartServer
          waitForServerStart fixture
          restartRecoveredPid <- readServerPid fixture
          readBundleCount fixture `shouldReturn` 4

          -- Cancelling the controller job stops the server and releases its port.
          cancel controllerJob
          waitUntil "the server exits" $ not <$> isProcessAlive restartRecoveredPid
          waitUntil "the server port is released" $ isPortAvailable serverPort
          clearServerPid fixture

    it "stops the server after a failed bundle and recovers on the next successful compile" $
      withGeneratedAppDirFixture $ \fixture -> do
        controller <- newServerProcessController
        generatedAppDir <- SP.parseAbsDir $ _generatedAppDirPath fixture
        let bundleScriptPath = serverDirPath fixture </> "bundle.js"
        originalBundleScript <- readFile' bundleScriptPath
        withAsync (runController controller generatedAppDir) $ \controllerJob -> do
          waitForServerStart fixture
          initialPid <- readServerPid fixture
          serverPort <- readServerPort fixture

          writeFile bundleScriptPath "process.exit(1);\n"
          notifySuccessfulCompileOrFail controller RebundleAndRestartServer
          waitUntil "the server exits" $ not <$> isProcessAlive initialPid
          waitUntil "the server port is released" $ isPortAvailable serverPort

          writeFile bundleScriptPath originalBundleScript
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller NoChange
          waitForServerStart fixture
          recoveredPid <- readServerPid fixture
          recoveredPid `shouldNotBe` initialPid
          readBundleCount fixture `shouldReturn` 2

          cancel controllerJob
          waitUntil "the server exits" $ not <$> isProcessAlive recoveredPid
          waitUntil "the server port is released" $ isPortAvailable serverPort
          clearServerPid fixture

    it "interrupts bundling when files change, and bundles again on the next compile" $
      withGeneratedAppDirFixture $ \fixture -> do
        controller <- newServerProcessController
        generatedAppDir <- SP.parseAbsDir $ _generatedAppDirPath fixture
        let bundleScriptPath = serverDirPath fixture </> "bundle.js"
        originalBundleScript <- readFile' bundleScriptPath
        withAsync (runController controller generatedAppDir) $ \controllerJob -> do
          waitForServerStart fixture
          initialPid <- readServerPid fixture

          -- The bundle only finishes in time if it's interrupted.
          appendFile bundleScriptPath "setTimeout(() => {}, 60000);\n"
          compileStartTime <- getCurrentTime
          withAsync (notifyServer controller $ Compiled $ SuccessfulCompile $ compileChangesWithEffect compileStartTime RebundleAndRestartServer) $ \compileHandling -> do
            waitUntil "the bundle starts" $ (== 2) <$> readBundleCount fixture
            notifyServer controller . FilesChanged =<< getCurrentTime
            timeout (secondsToMicroSeconds 10) (wait compileHandling) `shouldReturn` Just ()
          readServerPid fixture `shouldReturn` initialPid
          isProcessAlive initialPid `shouldReturn` True

          -- The server is out of date, so even a compile without server
          -- changes bundles it again.
          writeFile bundleScriptPath originalBundleScript
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller NoChange
          waitForServerStart fixture
          newPid <- readServerPid fixture
          newPid `shouldNotBe` initialPid
          readBundleCount fixture `shouldReturn` 3

          cancel controllerJob
          waitUntil "the server exits" $ not <$> isProcessAlive newPid
          clearServerPid fixture

    it "starts the server again after it crashes, on the next successful compile" $
      withGeneratedAppDirFixture $ \fixture -> do
        writeServerStartScript fixture crashingServerScript
        controller <- newServerProcessController
        generatedAppDir <- SP.parseAbsDir $ _generatedAppDirPath fixture
        withAsync (runController controller generatedAppDir) $ \controllerJob -> do
          waitUntil "crashed server pid file" $ doesFileExist $ serverPidFilePath fixture
          crashedPid <- readServerPid fixture
          waitUntil "server crash" $ not <$> isProcessAlive crashedPid

          writeServerStartScript fixture loopingServerScript
          clearServerPid fixture
          notifySuccessfulCompileOrFail controller NoChange
          waitForServerStart fixture
          newPid <- readServerPid fixture
          newPid `shouldNotBe` crashedPid
          readBundleCount fixture `shouldReturn` 2
          newPort <- readServerPort fixture

          cancel controllerJob
          waitUntil "the server port is released" $ isPortAvailable newPort
          clearServerPid fixture

    it "reports a server crash that happens while the server is being rebundled" $
      withGeneratedAppDirFixture $ \fixture -> do
        controller <- newServerProcessController
        generatedAppDir <- SP.parseAbsDir $ _generatedAppDirPath fixture
        output <- newIORef ""
        let bundleScriptPath = serverDirPath fixture </> "bundle.js"
            runControllerCollectingOutput =
              Job.run
                $ Job.captureOutput
                $ Job.observeOutput (\_ chunk -> modifyIORef' output (<> chunk))
                $ startServer serverRunConfig generatedAppDir controller
        withAsync runControllerCollectingOutput $ \controllerJob -> do
          waitForServerStart fixture
          -- The bundle kills the server, and gives Wasp a moment to notice.
          appendFile bundleScriptPath $
            unlines
              [ "process.kill(Number(require('node:fs').readFileSync('server.pid', 'utf8')), 'SIGKILL');",
                "setTimeout(() => {}, 500);"
              ]
          notifySuccessfulCompileOrFail controller RebundleAndRestartServer
          waitUntil "server crash report" $ ("Server process exited" `T.isInfixOf`) <$> readIORef output

          cancel controllerJob
          clearServerPid fixture

-- | Runs the controller until it's cancelled, discarding its output.
runController :: ServerProcessController -> Path' Abs (Dir GeneratedAppDir) -> IO (Void, Text)
runController controller generatedAppDir =
  Job.run $ Job.captureOutput $ startServer serverRunConfig generatedAppDir controller

serverRunConfig :: ServerRunConfig
serverRunConfig = makeServerRunConfig (makeAppComponentUrl 0 Nothing Nothing) "http://localhost:3000"

newtype GeneratedAppDirFixture = GeneratedAppDirFixture
  { _generatedAppDirPath :: FilePath
  }

withGeneratedAppDirFixture :: (GeneratedAppDirFixture -> IO ()) -> IO ()
withGeneratedAppDirFixture test = do
  generatedAppDirPath <- makeTempPath "wasp-server-controller-test"
  let fixture = GeneratedAppDirFixture generatedAppDirPath
  createDirectoryIfMissing True $ serverBundleDirPath fixture
  createDirectoryIfMissing True $ dotenvModuleDirPath fixture
  createNodeBin fixture "tsc" "exit 0"
  createNodeBin fixture "rollup" "exec node bundle.js"
  writeFile (serverDirPath fixture </> "bundle.js") bundleScript
  writeFile (dotenvModuleDirPath fixture </> "config.js") ""
  writeFile (serverDirPath fixture </> serverPortFileName) "0"
  writeServerStartScript fixture loopingServerScript
  test fixture `finally` cleanUpFixture fixture
  where
    bundleScript = "require('node:fs').appendFileSync('bundles.log', 'bundled\\n');\n"

-- | Creates an executable in the server's node_modules/.bin that runs the given
-- shell command, like npm does when installing a package with a bin.
createNodeBin :: GeneratedAppDirFixture -> String -> String -> IO ()
createNodeBin fixture binName command = do
  let binDir = serverDirPath fixture </> "node_modules" </> ".bin"
      binPath = binDir </> binName <.> exeExtension
  createDirectoryIfMissing True binDir
  writeFile binPath $ "#!/bin/sh\n" <> command <> "\n"
  getPermissions binPath >>= setPermissions binPath . setOwnerExecutable True

cleanUpFixture :: GeneratedAppDirFixture -> IO ()
cleanUpFixture fixture = do
  killPidFromFile $ serverPidFilePath fixture
  void (try (removeDirectoryRecursive $ _generatedAppDirPath fixture) :: IO (Either SomeException ()))
  where
    killPidFromFile pidFilePath = do
      exists <- doesFileExist pidFilePath
      when exists $ readProcessId pidFilePath >>= killProcess

loopingServerScript :: String
loopingServerScript =
  unlines
    [ "const fs = require('node:fs');",
      "const net = require('node:net');",
      "fs.writeFileSync('runtime-env.txt', [process.env.PORT, process.env.WASP_SERVER_URL, process.env.WASP_WEB_CLIENT_URL].join('\\n'));",
      "const port = Number(fs.readFileSync('" <> serverPortFileName <> "', 'utf8'));",
      "const server = net.createServer();",
      "server.listen(port, '127.0.0.1', () => {",
      "  fs.writeFileSync('" <> serverPortFileName <> "', String(server.address().port));",
      "  fs.writeFileSync('server.pid.tmp', String(process.pid));",
      "  fs.renameSync('server.pid.tmp', 'server.pid');",
      "});",
      "const stop = () => server.close(() => process.exit(0));",
      "process.on('SIGINT', stop);",
      "process.on('SIGTERM', stop);"
    ]

crashingServerScript :: String
crashingServerScript =
  unlines
    [ "const fs = require('node:fs');",
      "fs.writeFileSync('server.pid.tmp', String(process.pid));",
      "fs.renameSync('server.pid.tmp', 'server.pid');",
      "process.exit(1);"
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

readServerPid :: GeneratedAppDirFixture -> IO ProcessId
readServerPid fixture = readProcessId (serverPidFilePath fixture)

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
notifySuccessfulCompileOrFail controller serverEffect = do
  compileStartTime <- getCurrentTime
  failUnlessHandledInTime $ notifyServer controller $ Compiled $ SuccessfulCompile $ compileChangesWithEffect compileStartTime serverEffect

-- | The changes of a compile that started at the given time, that have the
-- given effect on the server.
compileChangesWithEffect :: UTCTime -> ServerEffect -> CompileChanges
compileChangesWithEffect compileStartTime serverEffect =
  CompileChanges
    { _compileStartTime = compileStartTime,
      _changedProjectFiles = [],
      _changedGeneratedAppPaths = case serverEffect of
        NoChange -> []
        RestartServer -> [Left [SP.relfile|server/.env|]]
        RebundleAndRestartServer -> [Left [SP.relfile|server/src/server.ts|]]
    }

notifyFailedCompileOrFail :: ServerProcessController -> IO ()
notifyFailedCompileOrFail controller = failUnlessHandledInTime $ notifyServer controller $ Compiled FailedCompile

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
              threadDelay (secondsToMicroSeconds 1 `div` 4)
              go $ remainingAttempts - 1
