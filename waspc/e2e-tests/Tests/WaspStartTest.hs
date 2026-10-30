module Tests.WaspStartTest (waspStartTest) where

import Control.Monad.Reader (asks)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    createTestWaspProject,
    inTestWaspProjectDir,
    waitUntil,
    waspCliCompile,
    waspCliStart,
    (~&&),
  )
import StrongPath (fromAbsDir)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | Each test case uses its own (client, server) ports, so they can run in
-- parallel.
waspStartTest :: Test
waspStartTest =
  Test
    "wasp-start"
    [ TestCase
        "fail-outside-project"
        (return [waspCliStartFails]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                runWaspStartAndStopIt (31000, 31001)
                  ++ [ return $ assertDirectoryExists ".wasp",
                       return $ assertDirectoryExists "node_modules"
                     ]
            ]
        ),
      TestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                waspCliCompile
                  : runWaspStartAndStopIt (31010, 31011)
            ]
        ),
      TestCase
        "fail-when-web-app-crashes"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ startWaspInBackground crashTestPorts,
                  return $ waitUntilAppIsListening crashTestPorts,
                  return crashWebApp,
                  return $ waitUntilWaspExits "Wasp didn't stop after the web app crashed.",
                  return "! wait \"$WASP_START_PID\"",
                  return $ "grep -q 'Web app failed' " ++ waspStartLogFile,
                  return $ waitUntilAppStopsListening crashTestPorts
                ]
            ]
        )
    ]
  where
    waspCliStartFails :: ShellCommand
    waspCliStartFails = "! $WASP_CLI_CMD start"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"

    crashTestPorts :: (Int, Int)
    crashTestPorts = (31020, 31021)

    -- Kills the Vite dev server of this test case's project.
    crashWebApp :: ShellCommand
    crashWebApp = "pkill -KILL -f \"$PWD/node_modules/.bin/vite\""

-- | Runs `wasp start` in the background until the app is up, and then stops
-- it with SIGINT. Checks that Wasp stops the processes it started, by checking
-- that they stop listening on the app's ports afterwards.
--
-- We send SIGINT only to Wasp, and not to its whole process group like Ctrl+C
-- in a terminal would, so that it's Wasp's job to stop the processes it
-- started.
runWaspStartAndStopIt :: (Int, Int) -> [ShellCommandBuilder WaspProjectContext ShellCommand]
runWaspStartAndStopIt ports =
  [ startWaspInBackground ports,
    return $ waitUntilAppIsListening ports,
    return stopWaspWithSigint,
    return $ waitUntilAppStopsListening ports
  ]
  where
    -- `$WASP_START_PID` might not be Wasp's own PID (e.g. when `$WASP_CLI_CMD`
    -- is a wrapper script), so we read it from the project lock file instead.
    stopWaspWithSigint :: ShellCommand
    stopWaspWithSigint =
      "kill -INT \"$(jq .pid .wasp/.projectlock)\""
        ~&& waitUntilWaspExits "Wasp didn't stop after SIGINT."

-- | Stores the PID of the background process in `$WASP_START_PID`.
--
-- Registers an EXIT trap that stops whatever is left of the app when the test
-- case ends, even if it fails. It also stops the server, which runs under
-- nodemon and so keeps running after Wasp stops it with SIGTERM.
startWaspInBackground :: (Int, Int) -> ShellCommandBuilder WaspProjectContext ShellCommand
startWaspInBackground (clientPort, serverPort) = do
  startCommand <- waspCliStart
  projectDir <- asks waspProjectDir
  let startWithPortsCommand =
        unwords
          [ startCommand,
            "--client-port",
            show clientPort,
            "--server-port",
            show serverPort
          ]
      projectLockFile = fromAbsDir projectDir ++ ".wasp/.projectlock"
      stopApp =
        unwords
          [ "kill \"$WASP_START_PID\" 2>/dev/null;",
            "kill \"$(jq .pid " ++ projectLockFile ++ " 2>/dev/null)\" 2>/dev/null;",
            "lsof -ti tcp:" ++ show clientPort ++ "," ++ show serverPort ++ " -sTCP:LISTEN | xargs kill 2>/dev/null"
          ]
  return $
    ("trap '" ++ stopApp ++ "' EXIT")
      ~&& ("{ " ++ startWithPortsCommand ++ " > " ++ waspStartLogFile ++ " 2>&1 & }")
      ~&& "WASP_START_PID=$!"

-- | Outside the project dir, so that writing to it doesn't trigger Wasp's file
-- watcher.
waspStartLogFile :: FilePath
waspStartLogFile = "../wasp-start.log"

waitUntilWaspExits :: String -> ShellCommand
waitUntilWaspExits = waitUntil 60 "! kill -0 \"$WASP_START_PID\" 2>/dev/null"

waitUntilAppIsListening :: (Int, Int) -> ShellCommand
waitUntilAppIsListening (clientPort, serverPort) =
  waitUntil
    300
    (isPortListening clientPort ~&& isPortListening serverPort)
    "The app didn't start listening on its ports."

-- | Returns as soon as the web app stops listening on its port, and fails if
-- it's still listening after 30 seconds.
--
-- Only checks the web app's port. The server runs under nodemon, which leaves
-- it running when it's stopped with SIGTERM, so it's stopped by the trap from
-- 'startWaspInBackground' instead.
waitUntilAppStopsListening :: (Int, Int) -> ShellCommand
waitUntilAppStopsListening (clientPort, _) =
  waitUntil
    30
    ("! " ++ isPortListening clientPort)
    "The web app kept listening on its port after Wasp stopped."

-- | Checks for a listening socket instead of making a request, so that it
-- can't hang on an app that accepts connections but doesn't respond.
isPortListening :: Int -> ShellCommand
isPortListening port = "lsof -ti tcp:" ++ show port ++ " -sTCP:LISTEN > /dev/null"
