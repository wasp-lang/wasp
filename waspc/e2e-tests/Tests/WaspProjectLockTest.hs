module Tests.WaspProjectLockTest (waspProjectLockTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    appendToPrismaFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliClean,
    waspCliCompile,
    waspCliInstall,
    (~&&),
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

waspProjectLockTest :: Test
waspProjectLockTest =
  Test
    "wasp-project-lock"
    [ TestCase
        "fail-while-another-wasp-command-holds-the-lock"
        -- To test the lock we need a Wasp command that holds it for as long as
        -- we want. We get that by running `wasp install` in the background and
        -- stalling the `npm install` it does, via an npm preinstall hook that
        -- waits for a signal file we create. In short:
        --   1. `wasp install` starts, acquires the lock, and gets stuck inside
        --      `npm install`, so it keeps holding the lock.
        --   2. Meanwhile, `wasp clean` must fail and name the holder's PID.
        --   3. We signal the hook to stop waiting, so `wasp install` finishes
        --      and drops the lock, and `wasp clean` works again.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ return addBlockingNpmPreinstallHook,
                  startWaspInstallInBackground,
                  return waitForLockToBeHeld,
                  assertWaspCleanFailsMentioningLockHolder,
                  return releaseLockAndAwaitWaspInstall,
                  -- The lock died with its holder, so the next command just works.
                  waspCliClean
                ]
            ]
        ),
      TestCase
        "db-commands-only-lock-when-the-generated-app-is-stale"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile taskPrismaModel,
                  waspCliCompile,
                  return addBlockingNpmPreinstallHook,
                  startWaspInstallInBackground,
                  return waitForLockToBeHeld,
                  assertDbMigrateSucceedsWithoutCompiling "first",
                  assertDbMigrateSucceedsWithoutCompiling "second",
                  appendToPrismaFile projectPrismaModel,
                  assertDbMigrateFailsMentioningLockHolder,
                  return releaseLockAndAwaitWaspInstall,
                  assertDbMigrateSucceedsAfterCompiling "third"
                ]
            ]
        ),
      TestCase
        "db-analysis-does-not-overwrite-a-concurrent-compilation"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile concurrentAnalysisSpec,
                  return startCompileWithPausedAnalyzer,
                  return $ waitForSignalFile ".wasp-e2e-analysis-ready",
                  return "export WASP_E2E_SPEC_TITLE=db-title",
                  assertDbMigrateFailsMentioningLockHolder,
                  return "touch .wasp-e2e-release-analysis && wait \"$WASP_E2E_COMPILER_PID\" && trap - EXIT",
                  return "grep -qF '<title>compile-title</title>' .wasp/out/sdk/wasp/client/app/layout.tsx"
                ]
            ]
        ),
      TestCase
        "succeed-with-leftover-lock-file"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ -- A lock file whose owner died: the file exists, but no
                  -- process holds the OS-level lock on it.
                  return "mkdir -p .wasp && printf 999999999 > .wasp/.projectlock",
                  waspCliClean
                ]
            ]
        )
    ]
  where
    -- The lock holder is a real `wasp install` process: it acquires the
    -- project lock and then runs `npm install` in the project dir, which this
    -- hook stalls until 'releaseLockAndAwaitWaspInstall' signals it (or ~120s
    -- pass). The hook also tells us, via the marker file, when `wasp install`
    -- is far enough along to be holding the lock.
    addBlockingNpmPreinstallHook :: ShellCommand
    addBlockingNpmPreinstallHook =
      "npm pkg set 'scripts.preinstall=" ++ preinstallHookScript ++ "'"

    preinstallHookScript :: String
    preinstallHookScript =
      "touch "
        ++ lockAcquiredMarkerFile
        ++ " && i=0 && until [ -f "
        ++ releaseLockSignalFile
        ++ " ]; do i=$((i+1)); [ \"$i\" -lt 600 ] || exit 1; sleep 0.2; done"

    startWaspInstallInBackground :: ShellCommandBuilder WaspProjectContext ShellCommand
    startWaspInstallInBackground = do
      installCommand <- waspCliInstall
      return $
        ("{ " ++ installCommand ++ " > .wasp-e2e-install.log 2>&1 & }")
          ~&& "WASP_E2E_LOCK_HOLDER_PID=$!"

    waitForLockToBeHeld :: ShellCommand
    waitForLockToBeHeld = waitForSignalFile lockAcquiredMarkerFile

    waitForSignalFile :: FilePath -> ShellCommand
    waitForSignalFile signalFile =
      "( i=0; until [ -f " ++ signalFile ++ " ]; do i=$((i+1)); [ \"$i\" -lt 600 ] || exit 1; sleep 0.2; done )"

    startCompileWithPausedAnalyzer :: ShellCommand
    startCompileWithPausedAnalyzer =
      "{ WASP_E2E_SPEC_TITLE=compile-title $WASP_CLI_CMD compile > .wasp-e2e-compile.log 2>&1 & }"
        ~&& "WASP_E2E_COMPILER_PID=$!"
        ~&& "trap 'touch .wasp-e2e-release-analysis; wait \"$WASP_E2E_COMPILER_PID\"' EXIT"

    concurrentAnalysisSpec :: T.Text
    concurrentAnalysisSpec =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { existsSync, writeFileSync } from "node:fs";

        if (process.env.WASP_E2E_SPEC_TITLE === "compile-title") {
          // The analyzer has written its result before Node emits beforeExit.
          // Hold it here until the second command has finished its analysis.
          process.once("beforeExit", () => {
            writeFileSync(".wasp-e2e-analysis-ready", "");
            const deadline = Date.now() + 120000;
            const sleepBuffer = new Int32Array(new SharedArrayBuffer(4));
            while (!existsSync(".wasp-e2e-release-analysis")) {
              if (Date.now() > deadline) throw new Error("Timed out waiting for the DB command");
              Atomics.wait(sleepBuffer, 0, 0, 50);
            }
          });
        }

        export default app({
          name: "concurrentAnalysis",
          title: process.env.WASP_E2E_SPEC_TITLE ?? "default-title",
          wasp: { version: "$textWaspVersion" },
          spec: [route("RootRoute", "/", page(MainPage))],
        });
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack $ show waspVersion

    -- The reported PID must be exactly the one the holding process wrote into
    -- the lock file.
    assertWaspCleanFailsMentioningLockHolder :: ShellCommandBuilder WaspProjectContext ShellCommand
    assertWaspCleanFailsMentioningLockHolder = do
      cleanCommand <- waspCliClean
      return $
        ("! " ++ cleanCommand ++ " > .wasp-e2e-clean.log 2>&1")
          ~&& "grep -qF \"Another Wasp command (PID $(cat .wasp/.projectlock)) is already running for this project.\" .wasp-e2e-clean.log"

    assertDbMigrateSucceedsWithoutCompiling :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertDbMigrateSucceedsWithoutCompiling migrationName =
      return $
        dbMigrateDevLoggingToFile migrationName
          ~&& ("grep -qF \"Your wasp project is already compiled and up to date.\" " ++ dbMigrateLogFile migrationName)
          ~&& ("! grep -qF \"Compiling wasp project\" " ++ dbMigrateLogFile migrationName)

    assertDbMigrateFailsMentioningLockHolder :: ShellCommandBuilder WaspProjectContext ShellCommand
    assertDbMigrateFailsMentioningLockHolder =
      return $
        ("! " ++ dbMigrateDevLoggingToFile "stale")
          ~&& ("grep -qF \"Another Wasp command (PID $(cat .wasp/.projectlock)) is already running for this project.\" " ++ dbMigrateLogFile "stale")

    assertDbMigrateSucceedsAfterCompiling :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertDbMigrateSucceedsAfterCompiling migrationName =
      return $
        dbMigrateDevLoggingToFile migrationName
          ~&& ("grep -qF \"Compiling wasp project\" " ++ dbMigrateLogFile migrationName)

    dbMigrateDevLoggingToFile :: String -> ShellCommand
    dbMigrateDevLoggingToFile migrationName =
      "$WASP_CLI_CMD db migrate-dev --name " ++ migrationName ++ " > " ++ dbMigrateLogFile migrationName ++ " 2>&1"

    dbMigrateLogFile :: String -> FilePath
    dbMigrateLogFile migrationName = ".wasp-e2e-migrate-" ++ migrationName ++ ".log"

    releaseLockAndAwaitWaspInstall :: ShellCommand
    releaseLockAndAwaitWaspInstall =
      "touch " ++ releaseLockSignalFile ++ " && wait \"$WASP_E2E_LOCK_HOLDER_PID\""

    lockAcquiredMarkerFile :: FilePath
    lockAcquiredMarkerFile = ".wasp-e2e-lock-acquired"

    -- Created by the test to tell the preinstall hook to stop waiting, which
    -- lets `wasp install` (and with it the lock) finish.
    releaseLockSignalFile :: FilePath
    releaseLockSignalFile = ".wasp-e2e-release-lock"

    taskPrismaModel :: T.Text
    taskPrismaModel =
      [trimming|
        model Task {
          id Int @id @default(autoincrement())
        }
      |]

    projectPrismaModel :: T.Text
    projectPrismaModel =
      [trimming|
        model Project {
          id Int @id @default(autoincrement())
        }
      |]
