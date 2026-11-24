module Tests.TranscriptTests (transcriptTests) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    writeToFile,
    (~&&),
  )
import StrongPath (relfile, (</>))
import TranscriptTest (Interaction (..), RuntimeOutput (..), TranscriptTest (..), WorkingDir (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

transcriptTests :: [TranscriptTest]
transcriptTests =
  [ outsideWaspProject "version" "version",
    outsideWaspProject "usage" "help",
    outsideWaspProject "unknown-command" "foo",
    outsideWaspProject "unknown-db-command" "db foo",
    (outsideWaspProject "completion-list" "completion:list")
      { command = "env COMP_LINE='wasp ' " ++ waspCli "completion:list"
      },
    -- Bash always sets `COMP_LINE` when it asks for completions, and Wasp
    -- doesn't expect to be asked without it.
    outsideWaspProject "unexpected-error" "completion:list",
    outsideWaspProject "compile-outside-project" "compile",
    (outsideWaspProject "new-with-answers" "new")
      { interactions =
          [ WaitForOutput "Enter the project name",
            WaitForOutput promptMarker,
            TypeLine "wasp-app",
            WaitForOutput "Choose a starter template",
            WaitForOutput promptMarker,
            TypeLine "minimal"
          ]
      },
    (outsideWaspProject "new-ctrl-d-at-prompt" "new")
      { interactions =
          [ WaitForOutput "Enter the project name",
            WaitForOutput promptMarker,
            PressCtrlD
          ]
      },
    uninstallDeclinedTest,
    inWaspProject "compile" [] "compile",
    inWaspProject
      "compile-wasp-errors-and-warnings"
      [replaceMainWaspTsFile mainWaspTsWithoutRoutes, writeProjectFile [relfile|.env|] "SOME_VAR=1"]
      "compile",
    inWaspProject
      "compile-type-error"
      [writeProjectFile [relfile|src/utils.ts|] "export const shouldBeNumber: number = 'wrong'"]
      "compile",
    inWaspProject "build" [setWaspDbToPSQL] "build",
    inWaspProject "clean" [] "clean",
    inWaspProject "deps" [] "deps",
    inWaspProject "dockerfile" [] "dockerfile",
    (startTest "start" (31100, 31101)) {runtimeOutput = CutOffAtRuntimeOutput},
    (startTest "start-ctrl-c" (31110, 31111)) {runtimeOutput = DropRuntimeOutput}
  ]
  where
    writeProjectFile file content = do
      context <- ask
      writeToFile (context.waspProjectDir </> file) content

    mainWaspTsWithoutRoutes :: T.Text
    mainWaspTsWithoutRoutes =
      [trimming|
        import { app } from "@wasp/spec";

        export default app({
          name: "waspApp",
          title: "wasp-app",
          wasp: { version: "$textWaspVersion" },
          spec: [],
        });
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion

outsideWaspProject :: String -> String -> TranscriptTest
outsideWaspProject testName waspCliArgs =
  TranscriptTest
    { name = testName,
      setupCommandsBuilder = return [],
      workingDir = ScenarioDir,
      command = waspCli waspCliArgs,
      interactions = [],
      runtimeOutput = NoRuntimeOutput
    }

-- | Runs the command in a fresh project made from the minimal starter.
inWaspProject :: String -> [ShellCommandBuilder WaspProjectContext ShellCommand] -> String -> TranscriptTest
inWaspProject testName projectSetupCommandBuilders waspCliArgs =
  TranscriptTest
    { name = testName,
      setupCommandsBuilder =
        sequence $
          createTestWaspProject minimalStarterTemplate
            : [inTestWaspProjectDir projectSetupCommandBuilders | not (null projectSetupCommandBuilders)],
      workingDir = WaspProjectDir,
      command = waspCli waspCliArgs,
      interactions = [],
      runtimeOutput = NoRuntimeOutput
    }

-- | Stops the app with Ctrl-C once it runs. Each test needs its own (client,
-- server) ports, so that the tests can run in parallel.
startTest :: String -> (Int, Int) -> TranscriptTest
startTest testName (clientPort, serverPort) =
  (inWaspProject testName [] startArgs)
    { interactions =
        [ WaitUntil $ isPortListening clientPort ~&& isPortListening serverPort,
          PressCtrlC
        ]
    }
  where
    startArgs = unwords ["start", "--client-port", show clientPort, "--server-port", show serverPort]

    isPortListening :: Int -> ShellCommand
    isPortListening port = "lsof -ti tcp:" ++ show port ++ " -sTCP:LISTEN > /dev/null"

-- | `wasp uninstall` deletes Wasp's files from the user's home directory, so
-- it gets a home directory of its own.
uninstallDeclinedTest :: TranscriptTest
uninstallDeclinedTest =
  (outsideWaspProject "uninstall-declined" "uninstall")
    { setupCommandsBuilder = return ["mkdir -p " ++ waspCacheDirInScenarioDir],
      command = unwords ["env", "HOME=\"$PWD/" ++ homeDirInScenarioDir ++ "\"", "XDG_CACHE_HOME=\"$PWD/" ++ cacheDirInScenarioDir ++ "\"", waspCli "uninstall"],
      interactions =
        [ WaitForOutput "Are you sure you want to continue?",
          WaitForOutput promptMarker,
          TypeLine "n"
        ]
    }
  where
    homeDirInScenarioDir = "home"
    cacheDirInScenarioDir = homeDirInScenarioDir ++ "/.cache"
    waspCacheDirInScenarioDir = cacheDirInScenarioDir ++ "/wasp"

waspCli :: String -> ShellCommand
waspCli args = "$WASP_CLI_CMD " ++ args

-- | Wasp prints it once it's ready to read the user's answer.
promptMarker :: String
promptMarker = "▸"
