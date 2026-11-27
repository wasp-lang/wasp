module Tests.WaspStartTest (test_waspStart) where

import Control.Monad (void)
import E2E
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

test_waspStart :: TestTree
test_waspStart =
  testGroup
    "wasp-start"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["start"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          runWaspStartAndStopIt SIGINT
          -- `wasp start` compiled the project and installed its dependencies.
          assertDirectoryExists $ SP.fromRelDir generatedAppDirInWaspProjectDir
          assertDirectoryExists nodeModulesDir,
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          runWaspStartAndStopIt SIGINT,
      test "stop-on-sigterm"
        $ withNewWaspProject minimalStarterTemplate
        $ runWaspStartAndStopIt SIGTERM,
      test "stop-on-sighup"
        $ withNewWaspProject minimalStarterTemplate
        $ runWaspStartAndStopIt SIGHUP,
      -- Closing the terminal sends SIGHUP to all the processes in it, not
      -- just to Wasp, so each of them reacts to it on its own.
      test "stop-on-terminal-hangup" $
        withNewWaspProject minimalStarterTemplate $ do
          -- Like a terminal, 'startInBackground' starts Wasp in its own process group.
          (app, _) <- startWaspInBackground
          signalProcessGroupOf SIGHUP app
          waitUntilExited app
          waitUntil "all processes Wasp started stop" settleTimeoutSeconds $
            not <$> processGroupOfExists app,
      test "fail-when-web-app-crashes" $
        withNewWaspProject minimalStarterTemplate $ do
          (app, ports) <- startWaspInBackground
          crashWebApp
          result <- assertExitsWithFailure app stopTimeoutSeconds
          waitUntilNotListening (appPortList ports)
          assertOutputContains "Web app failed" result
    ]
  where
    -- Kills the Vite dev server of the test's project. `pkill` fails when it
    -- finds no such process.
    crashWebApp :: TestM ()
    crashWebApp = do
      viteBin <- resolvePath "node_modules/.bin/vite"
      void . run $ program "pkill" ["-KILL", "-f", viteBin]

-- | Runs `wasp start` until the app is up, and then stops it with the signal.
-- Checks that Wasp stops the processes it started, by checking that nothing
-- listens on the app's ports afterwards.
--
-- We send the signal only to Wasp, and not to its whole process group like
-- Ctrl+C in a terminal would, so that it's Wasp's job to stop the processes it
-- started.
runWaspStartAndStopIt :: Signal -> TestM ()
runWaspStartAndStopIt sig = do
  (app, ports) <- startWaspInBackground
  getWaspPidFromLockFile >>= signalPid sig
  waitUntilStopped app (appPortList ports)

-- | Returns once the app listens on its ports.
startWaspInBackground :: TestM (RunningProcess, AppPorts)
startWaspInBackground = do
  ports <- allocateAppPorts
  app <- startInBackground $ waspCli ["start", "--client-port", show ports.clientPort, "--server-port", show ports.serverPort]
  waitUntilListening app (appPortList ports) appStartTimeoutSeconds
  return (app, ports)
