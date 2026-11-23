module Tests.WaspStartTest (waspStartTest) where

import Control.Monad (void)
import E2E
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

waspStartTest :: E2ETest
waspStartTest =
  testGroup
    "wasp-start"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["start"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          runWaspStartAndStopIt SIGINT
          -- `wasp start` compiled the project.
          assertDirectoryExists $ SP.fromRelDir generatedAppDirInWaspProjectDir,
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
          waitUntilStopped app []
          waitUntil "all processes Wasp started stop" 30 $
            not <$> processGroupOfExists app,
      test "fail-when-web-app-crashes" $
        withNewWaspProject minimalStarterTemplate $ do
          (app, ports) <- startWaspInBackground
          crashWebApp
          assertExitsWithFailure app 60
          waitUntilStopped app (appPortList ports)
          assertProcessOutputContains "Web app failed" app
    ]
  where
    -- Kills the Vite dev server of the test's project.
    crashWebApp :: TestM ()
    crashWebApp = do
      viteBin <- resolvePath "node_modules/.bin/vite"
      void . run $ command "pkill" ["-KILL", "-f", viteBin]

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
  void $ waitUntilStopped app (appPortList ports)

-- | Returns once the app listens on its ports.
startWaspInBackground :: TestM (RunningProcess, AppPorts)
startWaspInBackground = do
  ports <- allocateAppPorts
  app <- startInBackground $ waspCli ("start" : appPortArgs ports)
  waitUntilListening app (appPortList ports) (5 * 60)
  return (app, ports)
