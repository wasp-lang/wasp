module Generator.ServerGenerator.StartTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, cancel, withAsync)
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Void (Void)
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, doesFileExist, exeExtension, getPermissions, removeDirectoryRecursive, removeFile, setOwnerExecutable, setPermissions)
import System.FilePath ((<.>), (</>))
import System.IO (readFile')
import System.Info (os)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldNotBe, shouldReturn)
import Test.Process.Util (ProcessId, isPortAvailable, isProcessAlive, killProcess, makeTempPath, readProcessId, trim, waitUntil)
import Wasp.AppComponentUrl (makeAppComponentUrl)
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
    waitUntilCompilesHandled,
  )
import qualified Wasp.Job as Job
import Wasp.Util (secondsToMicroSeconds)

spec_ServerProcessController :: Spec
spec_ServerProcessController =
  -- The fake `tsc` and `rollup` that bundle the server are shell scripts, which
  -- Windows can't run.
  unless (os == "mingw32") $ describe "server process controller" $ do
    it "starts, restarts, and stops the server across compile outcomes" $
      withServerFixture loopingServerScript $ \fixture -> do
        initialPid <- waitForServerStart fixture
        serverPort <- readServerPort fixture
        readBundleCount fixture `shouldReturn` 1
        readFile' (serverDirPath fixture </> "runtime-env.txt")
          `shouldReturn` "0\nhttp://localhost:0\nhttp://localhost:3000"
        isPortAvailable serverPort `shouldReturn` False

        -- Client-only change: no bundle, no restart.
        notifySuccessfulCompile fixture NoChange
        waitUntilCompilesHandledOrFail fixture
        readBundleCount fixture `shouldReturn` 1
        readServerPid fixture `shouldReturn` initialPid
        isProcessAlive initialPid `shouldReturn` True

        -- Runtime-only change: restart without bundling.
        restartOnlyPid <- compileAndWaitForServer fixture RestartServer
        restartOnlyPid `shouldNotBe` initialPid
        waitForServerExit initialPid
        readBundleCount fixture `shouldReturn` 1

        -- Server source change: bundle + restart.
        restartedPid <- compileAndWaitForServer fixture RebundleAndRestartServer
        restartedPid `shouldNotBe` restartOnlyPid
        waitForServerExit restartOnlyPid
        readBundleCount fixture `shouldReturn` 2

        -- Failed compile stops the server and releases its port.
        notifyFailedCompile fixture
        waitForServerExit restartedPid
        waitForPortRelease serverPort

        -- Next successful compile brings the server back even without
        -- server-related changes.
        recoveredPid <- compileAndWaitForServer fixture NoChange
        readBundleCount fixture `shouldReturn` 3

        -- Restart-only also rebundles when there is no known-good server.
        notifyFailedCompile fixture
        waitForServerExit recoveredPid
        void $ compileAndWaitForServer fixture RestartServer
        readBundleCount fixture `shouldReturn` 4

    it "stops the server after a failed bundle and recovers on the next successful compile" $
      withServerFixture loopingServerScript $ \fixture -> do
        initialPid <- waitForServerStart fixture
        serverPort <- readServerPort fixture

        writeFile (bundleScriptPath fixture) "process.exit(1);\n"
        notifySuccessfulCompile fixture RebundleAndRestartServer
        waitForServerExit initialPid
        waitForPortRelease serverPort

        writeFile (bundleScriptPath fixture) bundleScript
        recoveredPid <- compileAndWaitForServer fixture NoChange
        recoveredPid `shouldNotBe` initialPid
        readBundleCount fixture `shouldReturn` 2

    it "interrupts bundling when files change, and bundles again on the next compile" $
      withServerFixture loopingServerScript $ \fixture -> do
        initialPid <- waitForServerStart fixture

        -- The bundle only finishes in time if it's interrupted, so the
        -- controller can't handle the next compile until it is.
        appendFile (bundleScriptPath fixture) "setTimeout(() => {}, 60000);\n"
        notifySuccessfulCompile fixture RebundleAndRestartServer
        waitUntil "the bundle starts" $ (== 2) <$> readBundleCount fixture
        notifyServer (_controller fixture) . FilesChanged =<< getCurrentTime
        waitUntilCompilesHandledOrFail fixture
        readServerPid fixture `shouldReturn` initialPid
        isProcessAlive initialPid `shouldReturn` True

        -- The server is out of date, so even a compile without server
        -- changes bundles it again.
        writeFile (bundleScriptPath fixture) bundleScript
        newPid <- compileAndWaitForServer fixture NoChange
        newPid `shouldNotBe` initialPid
        readBundleCount fixture `shouldReturn` 3

    it "handles the compiles queued while bundling together, rebundling if any of them failed" $
      withServerFixture loopingServerScript $ \fixture -> do
        void $ waitForServerStart fixture

        -- The bundle runs until we release it, so the compiles below queue up.
        appendFile (bundleScriptPath fixture) $
          unlines
            [ "const fs = require('node:fs');",
              "const waitForRelease = setInterval(() => fs.existsSync('release') && clearInterval(waitForRelease), 50);"
            ]
        notifySuccessfulCompile fixture RebundleAndRestartServer
        waitUntil "the bundle starts" $ (== 2) <$> readBundleCount fixture
        notifyFailedCompile fixture
        notifySuccessfulCompile fixture NoChange
        writeFile (serverDirPath fixture </> "release") ""

        waitUntil "the server is bundled again" $ (== 3) <$> readBundleCount fixture
        serverPid <- waitForServerStart fixture
        waitUntilCompilesHandledOrFail fixture
        isProcessAlive serverPid `shouldReturn` True

    it "starts the server again after it crashes, on the next successful compile" $
      withServerFixture crashingServerScript $ \fixture -> do
        waitUntil "crashed server pid file" $ doesFileExist $ serverPidFilePath fixture
        crashedPid <- readServerPid fixture
        waitForServerExit crashedPid

        writeServerStartScript fixture loopingServerScript
        newPid <- compileAndWaitForServer fixture NoChange
        newPid `shouldNotBe` crashedPid
        readBundleCount fixture `shouldReturn` 2

    it "restarts the server once the previous one releases its port, without another compile" $
      withServerFixture slowlyStoppingServerScript $ \fixture -> do
        initialPid <- waitForServerStart fixture
        serverPort <- readServerPort fixture

        -- The new server listens on the same port, so it only starts if the
        -- controller waits for the previous one to release it.
        restartedPid <- compileAndWaitForServer fixture RestartServer
        restartedPid `shouldNotBe` initialPid
        isProcessAlive initialPid `shouldReturn` False
        readServerPort fixture `shouldReturn` serverPort

    it "reports a server crash that happens while the server is being rebundled" $
      withServerFixture loopingServerScript $ \fixture -> do
        void $ waitForServerStart fixture
        -- The bundle kills the server, and gives Wasp a moment to notice.
        appendFile (bundleScriptPath fixture) $
          unlines
            [ "process.kill(Number(require('node:fs').readFileSync('server.pid', 'utf8')), 'SIGKILL');",
              "setTimeout(() => {}, 500);"
            ]
        notifySuccessfulCompile fixture RebundleAndRestartServer
        waitUntil "server crash report" $ ("Server process exited" `T.isInfixOf`) <$> readIORef (_output fixture)

serverRunConfig :: ServerRunConfig
serverRunConfig = makeServerRunConfig (makeAppComponentUrl 0 Nothing Nothing) "http://localhost:3000"

data ServerFixture = ServerFixture
  { _generatedAppDirPath :: FilePath,
    _controller :: ServerProcessController,
    _output :: IORef Text
  }

-- | Runs a controller against a fake generated app for the duration of a test.
-- Checks that cancellation stops the server and releases its port afterwards.
withServerFixture :: String -> (ServerFixture -> IO ()) -> IO ()
withServerFixture serverScript test = do
  generatedAppDirPath <- makeTempPath "wasp-server-controller-test"
  controller <- newServerProcessController
  output <- newIORef ""
  let fixture = ServerFixture generatedAppDirPath controller output
  createDirectoryIfMissing True $ serverBundleDirPath fixture
  createDirectoryIfMissing True $ dotenvModuleDirPath fixture
  createNodeBin fixture "tsc" "exit 0"
  createNodeBin fixture "rollup" "exec node bundle.js"
  writeFile (bundleScriptPath fixture) bundleScript
  writeFile (dotenvModuleDirPath fixture </> "config.js") ""
  writeFile (serverDirPath fixture </> serverPortFileName) "0"
  writeServerStartScript fixture serverScript
  generatedAppDir <- SP.parseAbsDir generatedAppDirPath
  let runController =
        Job.run
          $ Job.captureOutput
          $ Job.observeOutput (\_ chunk -> modifyIORef' output (<> chunk))
          $ startServer serverRunConfig generatedAppDir controller
  withAsync
    runController
    (\controllerJob -> test fixture >> stopController fixture controllerJob)
    `finally` cleanUpFixture fixture

bundleScript :: String
bundleScript = "require('node:fs').appendFileSync('bundles.log', 'bundled\\n');\n"

-- | Creates an executable in the server's node_modules/.bin that runs the given
-- shell command, like npm does when installing a package with a bin.
createNodeBin :: ServerFixture -> String -> String -> IO ()
createNodeBin fixture binName command = do
  let binDir = serverDirPath fixture </> "node_modules" </> ".bin"
      binPath = binDir </> binName <.> exeExtension
  createDirectoryIfMissing True binDir
  writeFile binPath $ "#!/bin/sh\n" <> command <> "\n"
  getPermissions binPath >>= setPermissions binPath . setOwnerExecutable True

cleanUpFixture :: ServerFixture -> IO ()
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

-- | Like 'loopingServerScript', but takes a while to release its port when
-- it's stopped.
slowlyStoppingServerScript :: String
slowlyStoppingServerScript =
  loopingServerScript
    <> unlines
      [ "process.removeAllListeners('SIGTERM');",
        "process.on('SIGTERM', () => setTimeout(stop, 1500));"
      ]

crashingServerScript :: String
crashingServerScript =
  unlines
    [ "const fs = require('node:fs');",
      "fs.writeFileSync('server.pid.tmp', String(process.pid));",
      "fs.renameSync('server.pid.tmp', 'server.pid');",
      "process.exit(1);"
    ]

writeServerStartScript :: ServerFixture -> String -> IO ()
writeServerStartScript fixture = writeFile (serverBundleDirPath fixture </> "server.js")

serverDirPath :: ServerFixture -> FilePath
serverDirPath fixture =
  _generatedAppDirPath fixture </> SP.fromRelDir ServerGenerator.Common.serverRootDirInGeneratedAppDir

serverBundleDirPath :: ServerFixture -> FilePath
serverBundleDirPath fixture = serverDirPath fixture </> "bundle"

dotenvModuleDirPath :: ServerFixture -> FilePath
dotenvModuleDirPath fixture = serverDirPath fixture </> "node_modules" </> "dotenv"

serverPidFilePath :: ServerFixture -> FilePath
serverPidFilePath fixture = serverDirPath fixture </> "server.pid"

bundlesLogFilePath :: ServerFixture -> FilePath
bundlesLogFilePath fixture = serverDirPath fixture </> "bundles.log"

bundleScriptPath :: ServerFixture -> FilePath
bundleScriptPath fixture = serverDirPath fixture </> "bundle.js"

serverPortFileName :: FilePath
serverPortFileName = "server-port.txt"

waitForServerStart :: ServerFixture -> IO ProcessId
waitForServerStart fixture = do
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
  readServerPid fixture

-- | Clears the previous PID so waiting cannot mistake the old server for the new one.
compileAndWaitForServer :: ServerFixture -> ServerEffect -> IO ProcessId
compileAndWaitForServer fixture serverEffect = do
  clearServerPid fixture
  notifySuccessfulCompile fixture serverEffect
  waitForServerStart fixture

waitForServerExit :: ProcessId -> IO ()
waitForServerExit pid = waitUntil "the server exits" $ not <$> isProcessAlive pid

waitForPortRelease :: String -> IO ()
waitForPortRelease port = waitUntil "the server port is released" $ isPortAvailable port

-- | Checks graceful shutdown before removing the PID used for fallback cleanup.
stopController :: ServerFixture -> Async (Void, Text) -> IO ()
stopController fixture controllerJob = do
  serverPid <- readServerPid fixture
  serverPort <- readServerPort fixture
  cancel controllerJob
  waitForServerExit serverPid
  waitForPortRelease serverPort
  clearServerPid fixture

readServerPid :: ServerFixture -> IO ProcessId
readServerPid fixture = readProcessId (serverPidFilePath fixture)

readServerPort :: ServerFixture -> IO String
readServerPort fixture = trim <$> readFile' (serverDirPath fixture </> serverPortFileName)

clearServerPid :: ServerFixture -> IO ()
clearServerPid fixture = removeFileIfExists $ serverPidFilePath fixture

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath

readBundleCount :: ServerFixture -> IO Int
readBundleCount fixture = do
  exists <- doesFileExist $ bundlesLogFilePath fixture
  if exists
    then length . lines <$> readFile' (bundlesLogFilePath fixture)
    else return 0

notifySuccessfulCompile :: ServerFixture -> ServerEffect -> IO ()
notifySuccessfulCompile fixture serverEffect = do
  compileStartTime <- getCurrentTime
  notifyServer (_controller fixture) $ Compiled $ SuccessfulCompile $ compileChangesWithEffect compileStartTime serverEffect

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

notifyFailedCompile :: ServerFixture -> IO ()
notifyFailedCompile fixture = notifyServer (_controller fixture) $ Compiled FailedCompile

-- | Waits until the controller has handled the compiles it was notified
-- about, failing instead of hanging if it doesn't.
waitUntilCompilesHandledOrFail :: ServerFixture -> IO ()
waitUntilCompilesHandledOrFail fixture =
  timeout (secondsToMicroSeconds 60) (waitUntilCompilesHandled $ _controller fixture)
    >>= maybe (expectationFailure "The controller didn't handle the compiles in time") return

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
