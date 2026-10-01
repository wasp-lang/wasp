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
    waspCliClean,
    waspCliCompile,
    waspCliInstall,
    (~&&),
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

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
    waitForLockToBeHeld =
      "( i=0; until [ -f " ++ lockAcquiredMarkerFile ++ " ]; do i=$((i+1)); [ \"$i\" -lt 600 ] || exit 1; sleep 0.2; done )"

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
        ("$WASP_CLI_CMD db migrate-dev --name " ++ migrationName ++ " > .wasp-e2e-migrate-" ++ migrationName ++ ".log 2>&1")
          ~&& ("grep -qF \"Your wasp project is already compiled and up to date.\" .wasp-e2e-migrate-" ++ migrationName ++ ".log")
          ~&& ("! grep -qF \"Compiling wasp project\" .wasp-e2e-migrate-" ++ migrationName ++ ".log")

    assertDbMigrateFailsMentioningLockHolder :: ShellCommandBuilder WaspProjectContext ShellCommand
    assertDbMigrateFailsMentioningLockHolder =
      return $
        "! $WASP_CLI_CMD db migrate-dev --name stale > .wasp-e2e-migrate-stale.log 2>&1"
          ~&& "grep -qF \"Another Wasp command (PID $(cat .wasp/.projectlock)) is already running for this project.\" .wasp-e2e-migrate-stale.log"

    assertDbMigrateSucceedsAfterCompiling :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
    assertDbMigrateSucceedsAfterCompiling migrationName =
      return $
        ("$WASP_CLI_CMD db migrate-dev --name " ++ migrationName ++ " > .wasp-e2e-migrate-" ++ migrationName ++ ".log 2>&1")
          ~&& ("grep -qF \"Compiling wasp project\" .wasp-e2e-migrate-" ++ migrationName ++ ".log")

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
