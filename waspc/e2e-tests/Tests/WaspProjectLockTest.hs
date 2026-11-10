module Tests.WaspProjectLockTest (waspProjectLockTest) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E
import NeatInterpolation (trimming)
import System.Exit (ExitCode (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WatcherStatus (..))

waspProjectLockTest :: E2ETest
waspProjectLockTest =
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
          lockHolderMessage <- getLockHolderMessage
          waspFails ["clean"] >>= assertOutputContains (lockHolderMessage "is already running for this project.")
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
          wasp ["clean"],
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
          lockHolderMessage <- getLockHolderMessage

          -- `wasp install` is not a watcher, so the db command waits for it to
          -- finish, and gives up after a while.
          result <- runExpectingFailure waspDbMigrateDevCommand
          assertOutputContains (lockHolderMessage "is working on this project right now.") result
          assertOutputContains waitingForCompilationMessage result

          setLockFileWatcherStatus CompilationFailed
          runExpectingFailure waspDbMigrateDevCommand
            >>= assertOutputContains (lockHolderMessage "is running for this project, but it failed to compile it.")

          -- While the watcher compiles, the db command waits for it, and goes
          -- on without compiling once the watcher is done.
          setLockFileWatcherStatus Compiling
          dbCommand <- startInBackground waspDbMigrateDevCommand
          waitForOutput dbCommand waitingForCompilationMessage 120
          setLockFileWatcherStatus UpToDate
          waitForExit dbCommand 120 >>= assertEqual "exit code of the db command" ExitSuccess
          getOutput dbCommand
            >>= assertTrue "the db command skips compilation"
              . T.isInfixOf "is already watching this project, skipping compilation."

          releaseLock lockHolder
    ]
  where
    waspDbMigrateDevCommand :: Command
    waspDbMigrateDevCommand = waspCli ["db", "migrate-dev", "--name", "foo"]

    waitingForCompilationMessage :: T.Text
    waitingForCompilationMessage = "Waiting for compilation to finish"

    -- The PID in the message must be exactly the one the holding process wrote
    -- into the lock file.
    getLockHolderMessage :: TestM (T.Text -> T.Text)
    getLockHolderMessage = do
      lockHolderPid <- waspPidFromLockFile
      return $ \message -> "A Wasp command (PID " <> T.pack (show lockHolderPid) <> ") " <> message

-- | Starts a real `wasp install`, which acquires the project lock and then runs
-- `npm install`, which a preinstall hook stalls until 'releaseLock' (or two
-- minutes pass). Returns once `wasp install` holds the lock.
startLockHolder :: TestM RunningProcess
startLockHolder = do
  writeToFile
    preinstallHookFile
    [trimming|
      import fs from "node:fs";

      fs.writeFileSync("${lockAcquiredMarkerFileText}", "");
      const deadline = Date.now() + 120_000;
      while (!fs.existsSync("${releaseLockSignalFileText}")) {
        if (Date.now() > deadline) process.exit(1);
        await new Promise((resolve) => setTimeout(resolve, 200));
      }
    |]
  void $ run addPreinstallHook
  lockHolder <- startInBackground $ waspCli ["install"]
  waitForFile lockAcquiredMarkerFile 120
  return lockHolder
  where
    lockAcquiredMarkerFileText = T.pack lockAcquiredMarkerFile
    releaseLockSignalFileText = T.pack releaseLockSignalFile

releaseLock :: RunningProcess -> TestM ()
releaseLock lockHolder = do
  writeToFile releaseLockSignalFile ""
  waitForExit lockHolder 120 >>= assertEqual "exit code of the lock holder" ExitSuccess

{- HLINT ignore addPreinstallHook "Avoid npm and npx" -}
addPreinstallHook :: Command
addPreinstallHook = command "npm" ["pkg", "set", "scripts.preinstall=node " ++ preinstallHookFile]

preinstallHookFile :: FilePath
preinstallHookFile = ".wasp-e2e-preinstall.mjs"

lockAcquiredMarkerFile :: FilePath
lockAcquiredMarkerFile = ".wasp-e2e-lock-acquired"

-- | Created by the test to tell the preinstall hook to stop waiting, which
-- lets `wasp install` (and with it the lock) finish.
releaseLockSignalFile :: FilePath
releaseLockSignalFile = ".wasp-e2e-release-lock"

writeLockFile :: ProjectLockData -> TestM ()
writeLockFile =
  -- 'writeToFile' overwrites the file in place, instead of replacing it with a
  -- new one. That's important, since the OS-level lock is tied to the file itself.
  writeToFile ".wasp/.projectlock" . TE.decodeUtf8 . BSL.toStrict . Aeson.encode

-- | Rewrites the lock file on behalf of the process holding the lock, keeping its PID.
setLockFileWatcherStatus :: WatcherStatus -> TestM ()
setLockFileWatcherStatus watcherStatus = do
  pid <- fromIntegral <$> waspPidFromLockFile
  writeLockFile ProjectLockData {pid, watcherStatus = Just watcherStatus}
