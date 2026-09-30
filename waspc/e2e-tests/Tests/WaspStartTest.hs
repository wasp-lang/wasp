module Tests.WaspStartTest (waspStartTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliCompile,
    waspCliStart,
    (~&&),
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

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
                runWaspStartAndStopIt "INT" (AppPorts 31000 31001)
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
                  : runWaspStartAndStopIt "INT" (AppPorts 31010 31011)
            ]
        ),
      TestCase
        "stop-on-sigterm"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $ runWaspStartAndStopIt "TERM" (AppPorts 31030 31031)
            ]
        ),
      TestCase
        "stop-on-sighup"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $ runWaspStartAndStopIt "HUP" (AppPorts 31040 31041)
            ]
        ),
      TestCase
        -- Closing the terminal sends SIGHUP to all the processes in it, not
        -- just to Wasp, so each of them reacts to it on its own.
        "stop-on-terminal-hangup"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ startWaspInBackgroundInOwnProcessGroup terminalHangupTestPorts,
                  return $ waitUntilAppIsListening terminalHangupTestPorts,
                  return "kill -HUP -- \"-$WASP_START_PID\"",
                  return $ waitUntilWaspExits "Wasp didn't stop after SIGHUP.",
                  return $
                    waitUntil
                      30
                      "! ps -A -o pgid= | grep -qw \"$WASP_START_PID\""
                      "Some of the processes Wasp started kept running after SIGHUP."
                ]
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

    crashTestPorts :: AppPorts
    crashTestPorts = AppPorts 31020 31021

    terminalHangupTestPorts :: AppPorts
    terminalHangupTestPorts = AppPorts 31050 31051

    -- Kills the Vite dev server of this test case's project.
    crashWebApp :: ShellCommand
    crashWebApp = "pkill -KILL -f \"$PWD/node_modules/.bin/vite\""

-- | Each test case uses its own ports, so they can run in parallel.
data AppPorts = AppPorts
  { clientPort :: Int,
    serverPort :: Int
  }

-- | Runs `wasp start` in the background until the app is up, and then stops
-- it with the given signal (e.g. @INT@). Checks that Wasp stops all the
-- processes it started, by checking that nothing keeps listening on the app's
-- ports afterwards.
--
-- We send the signal only to Wasp, and not to its whole process group like
-- Ctrl+C in a terminal would, so that it's Wasp's job to stop the processes it
-- started.
runWaspStartAndStopIt :: String -> AppPorts -> [ShellCommandBuilder WaspProjectContext ShellCommand]
runWaspStartAndStopIt signal ports =
  [ startWaspInBackground ports,
    return $ waitUntilAppIsListening ports,
    return stopWaspWithSignal,
    return $ waitUntilAppStopsListening ports
  ]
  where
    -- `$WASP_START_PID` might not be Wasp's own PID (e.g. when `$WASP_CLI_CMD`
    -- is a wrapper script), so we read it from the project lock file instead.
    stopWaspWithSignal :: ShellCommand
    stopWaspWithSignal =
      ("kill -" ++ signal ++ " \"$(cat .wasp/.projectlock)\"")
        ~&& waitUntilWaspExits ("Wasp didn't stop after SIG" ++ signal ++ ".")

-- | Stores the PID of the background process in `$WASP_START_PID`.
startWaspInBackground :: AppPorts -> ShellCommandBuilder WaspProjectContext ShellCommand
startWaspInBackground ports = do
  startCommand <- waspCliStartWithPorts ports
  return $
    ("{ " ++ startCommand ++ " > " ++ waspStartLogFile ++ " 2>&1 & }")
      ~&& "WASP_START_PID=$!"

-- | Like 'startWaspInBackground', but in a new process group, like a terminal
-- would do. The process group's ID is the same as `$WASP_START_PID`.
startWaspInBackgroundInOwnProcessGroup :: AppPorts -> ShellCommandBuilder WaspProjectContext ShellCommand
startWaspInBackgroundInOwnProcessGroup ports = do
  startCommand <- waspCliStartWithPorts ports
  return $
    ("{ perl -e 'setpgrp(0, 0); exec @ARGV' " ++ startCommand ++ " > " ++ waspStartLogFile ++ " 2>&1 & }")
      ~&& "WASP_START_PID=$!"

waspCliStartWithPorts :: AppPorts -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliStartWithPorts ports = do
  startCommand <- waspCliStart
  return $
    unwords
      [ startCommand,
        "--client-port",
        show ports.clientPort,
        "--server-port",
        show ports.serverPort
      ]

waspStartLogFile :: FilePath
waspStartLogFile = "wasp-start.log"

waitUntilWaspExits :: String -> ShellCommand
waitUntilWaspExits = waitUntil 60 "! kill -0 \"$WASP_START_PID\" 2>/dev/null"

waitUntilAppIsListening :: AppPorts -> ShellCommand
waitUntilAppIsListening ports =
  waitUntil
    300
    (isPortListening ports.clientPort ~&& isPortListening ports.serverPort)
    "The app didn't start listening on its ports."

waitUntilAppStopsListening :: AppPorts -> ShellCommand
waitUntilAppStopsListening ports =
  waitUntil
    30
    (("! " ++ isPortListening ports.clientPort) ~&& ("! " ++ isPortListening ports.serverPort))
    "Something kept listening on the app's ports after Wasp stopped."

isPortListening :: Int -> ShellCommand
isPortListening port = "curl -s -o /dev/null http://localhost:" ++ show port

-- | Polls every second until the condition succeeds, failing with the error
-- message if it doesn't within the given number of seconds.
waitUntil :: Int -> ShellCommand -> String -> ShellCommand
waitUntil timeoutSeconds condition errorMessage =
  "( i=0; until "
    ++ condition
    ++ "; do i=$((i+1)); [ \"$i\" -lt "
    ++ show timeoutSeconds
    ++ " ] || { echo "
    ++ show errorMessage
    ++ " >&2; exit 1; }; sleep 1; done )"
