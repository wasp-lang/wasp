module Tests.WaspProjectLockTest (waspProjectLockTest) where

import Control.Monad.Reader (ask)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    appendToFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliClean,
    waspCliCompile,
    (~&&),
  )
import StrongPath (File', Path', Rel, fromAbsFile, fromRelFile, relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WatcherStatus (..))
import Wasp.Project.Common (WaspProjectDir)

waspProjectLockTest :: Test
waspProjectLockTest =
  Test
    "wasp-project-lock"
    [ TestCase
        "fail-while-another-wasp-command-holds-the-lock"
        -- To test the lock we need a Wasp command that holds it for as long as
        -- we want. We get that by running `wasp compile` in the background and
        -- stalling it while it reads the Wasp Spec, with code in `main.wasp.ts`
        -- that waits for a signal file we create. In short:
        --   1. `wasp compile` starts, acquires the lock, and gets stuck inside
        --      `main.wasp.ts`, so it keeps holding the lock.
        --   2. Meanwhile, `wasp clean` must fail and name the holder's PID.
        --   3. We signal the code to stop waiting, so `wasp compile` finishes
        --      and drops the lock, and `wasp clean` works again.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ addBlockingCodeToWaspSpec,
                  return startWaspCompileInBackground,
                  return waitForLockToBeHeld,
                  assertWaspCleanFailsMentioningLockHolder,
                  return releaseLockAndAwaitWaspCompile,
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
                  return $
                    "mkdir -p .wasp"
                      ~&& writeLockFile ProjectLockData {pid = 999999999, watcherStatus = Just UpToDate},
                  waspCliClean
                ]
            ]
        ),
      TestCase
        "db-command-runs-alongside-watcher-only-while-generated-app-is-up-to-date"
        -- We can't run a real watcher (e.g. `wasp start`) here, so we fake one:
        -- we hold the lock with a stalled `wasp compile` (see the first test
        -- case), and then rewrite the lock file on its behalf, to go through
        -- the statuses a watcher reports.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ -- The db command won't compile the project while the lock
                  -- is held, so the generated app must already be there.
                  waspCliCompile,
                  addBlockingCodeToWaspSpec,
                  return startWaspCompileInBackground,
                  return waitForLockToBeHeld,
                  -- `wasp compile` is not a watcher, so the db command waits for
                  -- it to finish, and gives up after a while.
                  return $ assertWaspDbMigrateDevFailsWith "is working on this project right now.",
                  return $ "grep -qF " ++ waitingForCompilationMessage ++ " .wasp-e2e-db.log",
                  return $ setLockFileWatcherStatus CompilationFailed,
                  return $ assertWaspDbMigrateDevFailsWith "is running for this project, but it failed to compile it.",
                  -- While the watcher compiles, the db command waits for it, and
                  -- goes on without compiling once the watcher is done.
                  return $ setLockFileWatcherStatus Compiling,
                  return startWaspDbMigrateDevInBackground,
                  return $ waitUntil $ "grep -qF " ++ waitingForCompilationMessage ++ " .wasp-e2e-db.log",
                  return $ setLockFileWatcherStatus UpToDate,
                  return "wait \"$WASP_E2E_DB_PID\"",
                  return "grep -qF 'is already watching this project, skipping compilation.' .wasp-e2e-db.log",
                  return releaseLockAndAwaitWaspCompile
                ]
            ]
        )
    ]
  where
    -- Stalls the first Wasp command that reads the Wasp Spec until
    -- 'releaseLockAndAwaitWaspCompile' signals it, or ~120s pass. Its marker
    -- file tells us `wasp compile` holds the lock. Later commands find the
    -- marker and don't wait.
    addBlockingCodeToWaspSpec :: ShellCommandBuilder WaspProjectContext ShellCommand
    addBlockingCodeToWaspSpec = do
      context <- ask
      let jsStringOfFileInProjectDir file = show $ fromAbsFile $ context.waspProjectDir </> file
      appendToFile "main.wasp.ts" $
        T.unlines
          [ "import { execFileSync as waspE2eExecFileSync } from 'node:child_process';",
            "import { existsSync as waspE2eExistsSync, writeFileSync as waspE2eWriteFileSync } from 'node:fs';",
            "if (!waspE2eExistsSync(" <> T.pack (jsStringOfFileInProjectDir lockAcquiredMarkerFile) <> ")) {",
            "  waspE2eWriteFileSync(" <> T.pack (jsStringOfFileInProjectDir lockAcquiredMarkerFile) <> ", '');",
            "  for (let i = 0; !waspE2eExistsSync(" <> T.pack (jsStringOfFileInProjectDir releaseLockSignalFile) <> "); i++) {",
            "    if (i >= 600) throw new Error('Nobody released the lock.');",
            "    waspE2eExecFileSync('sleep', ['0.2']);",
            "  }",
            "}"
          ]

    startWaspCompileInBackground :: ShellCommand
    startWaspCompileInBackground =
      "{ $WASP_CLI_CMD compile > .wasp-e2e-compile.log 2>&1 & }"
        ~&& "WASP_E2E_LOCK_HOLDER_PID=$!"

    waitForLockToBeHeld :: ShellCommand
    waitForLockToBeHeld = waitUntil $ "[ -f " ++ fromRelFile lockAcquiredMarkerFile ++ " ]"

    -- Polls the given condition for up to ~120s.
    waitUntil :: ShellCommand -> ShellCommand
    waitUntil condition =
      "( i=0; until " ++ condition ++ "; do i=$((i+1)); [ \"$i\" -lt 600 ] || exit 1; sleep 0.2; done )"

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
      "grep -qF \"A Wasp command (PID " ++ lockOwnerPid ++ ") " ++ messageAfterLockHolder ++ "\" " ++ logFile

    -- Shell expression for the PID that the lock holder wrote into the lock file.
    lockOwnerPid :: String
    lockOwnerPid = "$(jq .pid .wasp/.projectlock)"

    writeLockFile :: ProjectLockData -> ShellCommand
    writeLockFile projectLockData =
      "printf %s '" ++ BLC.unpack (Aeson.encode projectLockData) ++ "' > .wasp/.projectlock"

    -- Rewrites the lock file on behalf of the process holding the lock, keeping
    -- its PID.
    -- NOTE: The redirection rewrites the lock file in place. That's important
    -- because the OS-level lock is tied to the file itself, so replacing the
    -- file with a new one would leave us with an unlocked lock file.
    setLockFileWatcherStatus :: WatcherStatus -> ShellCommand
    setLockFileWatcherStatus watcherStatus =
      ( "WASP_E2E_LOCK_FILE_CONTENTS=$(jq -c --argjson watcherStatus '"
          ++ BLC.unpack (Aeson.encode watcherStatus)
          ++ "' '.watcherStatus = $watcherStatus' .wasp/.projectlock)"
      )
        ~&& "printf %s \"$WASP_E2E_LOCK_FILE_CONTENTS\" > .wasp/.projectlock"

    startWaspDbMigrateDevInBackground :: ShellCommand
    startWaspDbMigrateDevInBackground =
      ("{ " ++ waspCliDbMigrateDev ++ " > .wasp-e2e-db.log 2>&1 & }")
        ~&& "WASP_E2E_DB_PID=$!"

    waspCliDbMigrateDev :: ShellCommand
    waspCliDbMigrateDev = "$WASP_CLI_CMD db migrate-dev --name foo"

    waitingForCompilationMessage :: String
    waitingForCompilationMessage = "'Waiting for compilation to finish'"

    releaseLockAndAwaitWaspCompile :: ShellCommand
    releaseLockAndAwaitWaspCompile =
      "touch " ++ fromRelFile releaseLockSignalFile ++ " && wait \"$WASP_E2E_LOCK_HOLDER_PID\""

    lockAcquiredMarkerFile :: Path' (Rel WaspProjectDir) File'
    lockAcquiredMarkerFile = [relfile|.wasp-e2e-lock-acquired|]

    -- Created by the test to tell the code in `main.wasp.ts` to stop waiting,
    -- which lets `wasp compile` finish and release the lock.
    releaseLockSignalFile :: Path' (Rel WaspProjectDir) File'
    releaseLockSignalFile = [relfile|.wasp-e2e-release-lock|]
