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
                runWaspStartAndStopIt (AppPorts 31000 31001)
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
                  : runWaspStartAndStopIt (AppPorts 31010 31011)
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

    -- Kills the Vite dev server of this test case's project.
    crashWebApp :: ShellCommand
    crashWebApp = "pkill -KILL -f \"$PWD/node_modules/.bin/vite\""

-- | Each test case uses its own ports, so they can run in parallel.
data AppPorts = AppPorts
  { clientPort :: Int,
    serverPort :: Int
  }

-- | Runs `wasp start` in the background until the app is up, and then stops
-- it with SIGINT. Checks that Wasp stops all the processes it started, by
-- checking that nothing keeps listening on the app's ports afterwards.
--
-- We send SIGINT only to Wasp, and not to its whole process group like Ctrl+C
-- in a terminal would, so that it's Wasp's job to stop the processes it
-- started.
runWaspStartAndStopIt :: AppPorts -> [ShellCommandBuilder WaspProjectContext ShellCommand]
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
      "kill -INT \"$(cat .wasp/.projectlock)\""
        ~&& waitUntilWaspExits "Wasp didn't stop after SIGINT."

-- | Stores the PID of the background process in `$WASP_START_PID`.
startWaspInBackground :: AppPorts -> ShellCommandBuilder WaspProjectContext ShellCommand
startWaspInBackground ports = do
  startCommand <- waspCliStart
  let startWithPortsCommand =
        unwords
          [ startCommand,
            "--client-port",
            show ports.clientPort,
            "--server-port",
            show ports.serverPort
          ]
  return $
    ("{ " ++ startWithPortsCommand ++ " > " ++ waspStartLogFile ++ " 2>&1 & }")
      ~&& "WASP_START_PID=$!"

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
