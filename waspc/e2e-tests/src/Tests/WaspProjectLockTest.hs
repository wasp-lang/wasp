module Tests.WaspProjectLockTest (test_waspProjectLock) where

import Control.Monad (void)
import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WatcherStatus (..))
import Wasp.Project.Common (dotWaspDirInWaspProjectDir)

test_waspProjectLock :: TestTree
test_waspProjectLock =
  testGroup
    "wasp-project-lock"
    [ -- To test the lock we need a Wasp command that holds it for as long as
      -- we want. We get that by running `wasp install` in the background and
      -- stalling the `npm install` it does, via an npm preinstall hook that
      -- waits for a signal file we create. In short:
      --   1. `wasp install` starts, acquires the lock, and gets stuck inside
      --      `npm install`, so it keeps holding the lock.
      --   2. Meanwhile, `wasp clean` must fail and name the holder's PID.
      --   3. We signal the hook to stop waiting, so `wasp install` finishes
      --      and drops the lock, and `wasp clean` works again.
      test "fail-while-another-wasp-command-holds-the-lock" $
        withNewWaspProject minimalStarterTemplate $ do
          lockHolder <- startLockHolder
          lockHolderPid <- getWaspPidFromLockFile
          waspFails ["clean"] >>= assertOutputContains (lockHolderMessage lockHolderPid "is already running for this project.")
          releaseLock lockHolder
          -- The lock died with its holder, so the next command just works.
          wasp ["clean"],
      test "succeed-with-leftover-lock-file" $
        withNewWaspProject minimalStarterTemplate $ do
          -- A lock file whose owner died: the file exists, but no process
          -- holds the OS-level lock on it.
          -- Even if that owner was a watcher that had the generated app up to
          -- date, nobody is keeping it up to date anymore.
          writeLockFile ProjectLockData {pid = 999999999, watcherStatus = Just UpToDate}
          wasp ["clean"]
          -- `.wasp` holds the lock file we wrote, so it's only gone if `wasp clean` ran.
          assertDirectoryDoesNotExist $ SP.fromRelDir dotWaspDirInWaspProjectDir,
      -- We can't run a real watcher (e.g. `wasp start`) here, so we fake one:
      -- we hold the lock with a stalled `wasp install` (see the first test),
      -- and then rewrite the lock file on its behalf, to go through the
      -- statuses a watcher reports.
      test "db-command-runs-alongside-watcher-only-while-generated-app-is-up-to-date" $
        withNewWaspProject minimalStarterTemplate $ do
          -- The db command won't compile the project while the lock is held,
          -- so the generated app must already be there.
          wasp ["compile"]
          lockHolder <- startLockHolder
          lockHolderPid <- getWaspPidFromLockFile

          -- `wasp install` holds the lock but isn't a watcher. The db command
          -- waits for it a while before it gives up.
          step "the db command gives up waiting for a non-watcher lock holder" $ do
            result <- runExpectingFailure waspDbMigrateDevCommand
            assertOutputContains (lockHolderMessage lockHolderPid "is working on this project right now.") result
            assertOutputContains waitingForCompilationMessage result
          step "the db command fails while the watcher has a failed compilation" $ do
            setLockFileWatcherStatus lockHolder CompilationFailed
            runExpectingFailure waspDbMigrateDevCommand
              >>= assertOutputContains (lockHolderMessage lockHolderPid "is running for this project, but it failed to compile it.")
          step "the db command waits while the watcher compiles, and goes on without compiling once it's done" $ do
            setLockFileWatcherStatus lockHolder Compiling
            dbCommand <- startInBackground waspDbMigrateDevCommand
            waitForOutput dbCommand waitingForCompilationMessage lockHolderDeadlineSeconds
            setLockFileWatcherStatus lockHolder UpToDate
            assertExitsSuccessfully dbCommand lockHolderDeadlineSeconds
              >>= assertOutputContains "is already watching this project, skipping compilation."

          releaseLock lockHolder
    ]
  where
    waspDbMigrateDevCommand :: Command
    waspDbMigrateDevCommand = waspCli ["db", "migrate-dev", "--name", "alongside_lock_holder"]

    waitingForCompilationMessage :: T.Text
    waitingForCompilationMessage = "Waiting for compilation to finish"

    -- The PID in the message must be exactly the one the holding process wrote
    -- into the lock file.
    lockHolderMessage :: Int -> T.Text -> T.Text
    lockHolderMessage lockHolderPid message = "A Wasp command (PID " <> T.pack (show lockHolderPid) <> ") " <> message

-- | Starts a real `wasp install`, which acquires the project lock and then runs
-- `npm install`, which a preinstall hook stalls until 'releaseLock' (or until
-- 'lockHolderDeadlineSeconds' pass). Returns once `wasp install` holds the lock.
startLockHolder :: TestM RunningProcess
startLockHolder = do
  writeToFile
    preinstallHookFile
    [trimming|
      import fs from "node:fs";

      fs.writeFileSync("${lockAcquiredMarkerFileText}", "");
      const deadline = Date.now() + ${lockHolderDeadlineMillisecondsText};
      while (!fs.existsSync("${releaseLockSignalFileText}")) {
        if (Date.now() > deadline) process.exit(1);
        await new Promise((resolve) => setTimeout(resolve, 200));
      }
    |]
  run addPreinstallHookCommand
  lockHolder <- startInBackground $ waspCli ["install"]
  waitForFile lockHolder lockAcquiredMarkerFile lockAcquiredTimeoutSeconds
  return lockHolder
  where
    -- `wasp install` takes the lock, then `npm install` gets to the hook.
    lockAcquiredTimeoutSeconds = 120
    lockAcquiredMarkerFileText = T.pack lockAcquiredMarkerFile
    releaseLockSignalFileText = T.pack releaseLockSignalFile
    lockHolderDeadlineMillisecondsText = T.pack . show $ lockHolderDeadlineSeconds * 1000

-- | How long the preinstall hook stalls at most, so that a test that never
-- calls 'releaseLock' can't stall it forever. Everything a test does while it
-- holds the lock must fit in it, so it's also how long those steps can wait.
lockHolderDeadlineSeconds :: Int
lockHolderDeadlineSeconds = 120

releaseLock :: RunningProcess -> TestM ()
releaseLock lockHolder = do
  writeToFile releaseLockSignalFile ""
  void $ assertExitsSuccessfully lockHolder installFinishTimeoutSeconds
  where
    -- The rest of `npm install`, after the hook.
    installFinishTimeoutSeconds = 120

{- HLINT ignore addPreinstallHookCommand "Avoid npm and npx" -}
addPreinstallHookCommand :: Command
addPreinstallHookCommand = command "npm" ["pkg", "set", "scripts.preinstall=node " ++ preinstallHookFile]

preinstallHookFile :: FilePath
preinstallHookFile = ".wasp-e2e-preinstall.mjs"

lockAcquiredMarkerFile :: FilePath
lockAcquiredMarkerFile = ".wasp-e2e-lock-acquired"

-- | Created by the test to tell the preinstall hook to stop waiting, which
-- lets `wasp install` (and with it the lock) finish.
releaseLockSignalFile :: FilePath
releaseLockSignalFile = ".wasp-e2e-release-lock"

-- | Rewrites the lock file on behalf of the process holding the lock, keeping its PID.
--
-- Fails if the lock holder already exited (e.g. after
-- 'lockHolderDeadlineSeconds'), since the steps after this one would then fail
-- for an unrelated reason.
setLockFileWatcherStatus :: RunningProcess -> WatcherStatus -> TestM ()
setLockFileWatcherStatus lockHolder watcherStatus = do
  failIfExited lockHolder "the test was done with the lock"
  lockData <- readLockFile
  writeLockFile lockData {watcherStatus = Just watcherStatus}
