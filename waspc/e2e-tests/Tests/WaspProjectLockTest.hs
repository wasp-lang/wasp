module Tests.WaspProjectLockTest (waspProjectLockTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
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
        "succeed-with-leftover-lock-file"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ -- A lock file whose owner died: the file exists, but no
                  -- process holds the OS-level lock on it.
                  -- Even if that owner was a watcher that had the generated app
                  -- up to date, nobody is keeping it up to date anymore.
                  return $ "mkdir -p .wasp" ~&& writeLockFileAsWatcher "999999999" "UpToDate",
                  waspCliClean
                ]
            ]
        ),
      TestCase
        "db-command-runs-alongside-watcher-only-while-generated-app-is-up-to-date"
        -- We can't run a real watcher (e.g. `wasp start`) here, so we fake one:
        -- we hold the lock with a stalled `wasp install` (see the first test
        -- case), and then rewrite the lock file on its behalf, to go through
        -- the statuses a watcher reports.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ -- The db command won't compile the project while the lock
                  -- is held, so the generated app must already be there.
                  waspCliCompile,
                  return addBlockingNpmPreinstallHook,
                  startWaspInstallInBackground,
                  return waitForLockToBeHeld,
                  -- `wasp install` is not a watcher.
                  return $ assertWaspDbMigrateDevFailsWith "is already running for this project. Stop it before running this command.",
                  return $ writeLockFileAsWatcher lockOwnerPid "Compiling",
                  return $ assertWaspDbMigrateDevFailsWith "is compiling this project right now.",
                  return $ writeLockFileAsWatcher lockOwnerPid "CompilationFailed",
                  return $ assertWaspDbMigrateDevFailsWith "is running for this project, but it failed to compile it.",
                  return $ writeLockFileAsWatcher lockOwnerPid "UpToDate",
                  return $ waspCliDbMigrateDev ++ " > .wasp-e2e-db.log 2>&1",
                  return "grep -qF 'Skipping compilation' .wasp-e2e-db.log",
                  return releaseLockAndAwaitWaspInstall
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
          ~&& assertLogMentionsLockHolder ".wasp-e2e-clean.log" "is already running for this project."

    assertWaspDbMigrateDevFailsWith :: String -> ShellCommand
    assertWaspDbMigrateDevFailsWith messageAfterLockHolder =
      ("! " ++ waspCliDbMigrateDev ++ " > .wasp-e2e-db.log 2>&1")
        ~&& assertLogMentionsLockHolder ".wasp-e2e-db.log" messageAfterLockHolder

    assertLogMentionsLockHolder :: FilePath -> String -> ShellCommand
    assertLogMentionsLockHolder logFile messageAfterLockHolder =
      "grep -qF \"Another Wasp command (PID " ++ lockOwnerPid ++ ") " ++ messageAfterLockHolder ++ "\" " ++ logFile

    -- Shell expression for the PID that the lock holder wrote into the lock file.
    lockOwnerPid :: String
    lockOwnerPid = "$(sed -E 's/.*\"pid\":([0-9]+).*/\\1/' .wasp/.projectlock)"

    -- NOTE: The redirection rewrites the lock file in place. That's important
    -- because the OS-level lock is tied to the file itself, so replacing the
    -- file with a new one would leave us with an unlocked lock file.
    writeLockFileAsWatcher :: String -> String -> ShellCommand
    writeLockFileAsWatcher pid watcherStatus =
      "WASP_E2E_LOCK_FILE_CONTENTS=\"{\\\"pid\\\":"
        ++ pid
        ++ ",\\\"watcherStatus\\\":\\\""
        ++ watcherStatus
        ++ "\\\"}\""
          ~&& "printf %s \"$WASP_E2E_LOCK_FILE_CONTENTS\" > .wasp/.projectlock"

    waspCliDbMigrateDev :: ShellCommand
    waspCliDbMigrateDev = "$WASP_CLI_CMD db migrate-dev --name foo"

    releaseLockAndAwaitWaspInstall :: ShellCommand
    releaseLockAndAwaitWaspInstall =
      "touch " ++ releaseLockSignalFile ++ " && wait \"$WASP_E2E_LOCK_HOLDER_PID\""

    lockAcquiredMarkerFile :: FilePath
    lockAcquiredMarkerFile = ".wasp-e2e-lock-acquired"

    -- Created by the test to tell the preinstall hook to stop waiting, which
    -- lets `wasp install` (and with it the lock) finish.
    releaseLockSignalFile :: FilePath
    releaseLockSignalFile = ".wasp-e2e-release-lock"
