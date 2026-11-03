module Tests.WaspStartTest (waspStartTest) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import E2e
import E2e.Ports (AppPorts, appPortArgs, appPortList)
import qualified E2e.Ports as Ports
import E2e.Signals (processGroupExists)
import System.Exit (ExitCode (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspStartTest :: E2eTest
waspStartTest =
  testGroup
    "wasp-start"
    [ test "fail-outside-project" $ waspFails ["start"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          runWaspStartAndStopIt SIGINT Ports.waspStartUncompiledProject
          assertDirectoryExists ".wasp"
          assertDirectoryExists "node_modules",
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          runWaspStartAndStopIt SIGINT Ports.waspStartCompiledProject,
      test "stop-on-sigterm"
        $ withNewWaspProject minimalStarterTemplate
        $ runWaspStartAndStopIt SIGTERM Ports.waspStartSigterm,
      test "stop-on-sighup"
        $ withNewWaspProject minimalStarterTemplate
        $ runWaspStartAndStopIt SIGHUP Ports.waspStartSighup,
      -- Closing the terminal sends SIGHUP to all the processes in it, not
      -- just to Wasp, so each of them reacts to it on its own.
      test "stop-on-terminal-hangup" $
        withNewWaspProject minimalStarterTemplate $ do
          let ports = Ports.waspStartTerminalHangup
          -- Like a terminal, 'startInBackground' starts Wasp in its own process group.
          app <- startWaspInBackground ports
          signalProcessGroupOf SIGHUP app
          waitForExit app 60
          waitUntil "all processes Wasp started stop" 30 $
            not <$> liftIO (processGroupExists app.processGroupId),
      test "fail-when-web-app-crashes" $
        withNewWaspProject minimalStarterTemplate $ do
          let ports = Ports.waspStartWebAppCrash
          app <- startWaspInBackground ports
          crashWebApp
          exitCode <- waitForExit app 60
          assertTrue "Wasp exits with an error" (exitCode /= ExitSuccess)
          output <- getBackgroundOutput app
          assertTrue "Wasp reports that the web app failed" ("Web app failed" `T.isInfixOf` output)
          waitUntilNotListening (appPortList ports) 30
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
runWaspStartAndStopIt :: Signal -> AppPorts -> TestM ()
runWaspStartAndStopIt sig ports = do
  app <- startWaspInBackground ports
  waspPidFromLockFile >>= signal sig
  waitForExit app 60
  waitUntilNotListening (appPortList ports) 30

-- | Returns once the app listens on its ports.
startWaspInBackground :: AppPorts -> TestM BackgroundProcess
startWaspInBackground ports = do
  app <- startInBackground $ waspCli ("start" : appPortArgs ports)
  waitUntilListening (appPortList ports) 300
  return app
