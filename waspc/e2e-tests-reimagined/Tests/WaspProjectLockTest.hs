module Tests.WaspProjectLockTest (waspProjectLockTests) where

import Control.Monad (void)
import qualified Data.Text as T
import E2e
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspProjectLockTests :: [E2eTest]
waspProjectLockTests =
  testGroup
    "wasp-project-lock"
    [ -- To test the lock we need a Wasp command that holds it for as long as we
      -- want. We get that by running `wasp install` in the background and
      -- stalling the `npm install` it runs, with a preinstall hook that waits
      -- for a signal file. In short:
      --   1. `wasp install` acquires the lock and gets stuck in `npm install`.
      --   2. Meanwhile, `wasp clean` must fail and name the holder's PID.
      --   3. We signal the hook to stop waiting, so `wasp install` finishes and
      --      releases the lock, and `wasp clean` works again.
      test "fail-while-another-wasp-command-holds-the-lock" $
        withNewWaspProject minimalStarterTemplate $ do
          addBlockingPreinstallHook
          waspInstall <- startInBackground $ waspCli ["install"]
          waitForFile lockAcquiredMarkerFile 120

          lockHolderPid <- T.strip <$> readFromFile ".wasp/.projectlock"
          waspFails ["clean"]
            >>= assertOutputContains ("Another Wasp command (PID " <> lockHolderPid <> ") is already running for this project.")

          writeToFile releaseLockSignalFile ""
          waitForSuccessfulExit waspInstall
          -- The lock was released with its holder, so the next command just works.
          wasp ["clean"],
      test "succeed-with-leftover-lock-file" $
        withNewWaspProject minimalStarterTemplate $ do
          -- A lock file whose owner died: the file exists, but no process
          -- holds the OS-level lock on it.
          writeToFile ".wasp/.projectlock" "999999999"
          wasp ["clean"]
    ]
  where
    -- The hook tells us when `wasp install` is far enough to hold the lock,
    -- and then waits until we signal it to continue (or two minutes pass).
    addBlockingPreinstallHook :: TestM ()
    addBlockingPreinstallHook = do
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
      void . run $ command "npm" ["pkg", "set", "scripts.preinstall=node " ++ preinstallHookFile]

    preinstallHookFile = ".wasp-e2e-preinstall.mjs"

    lockAcquiredMarkerFile = ".wasp-e2e-lock-acquired"
    lockAcquiredMarkerFileText = T.pack lockAcquiredMarkerFile

    releaseLockSignalFile = ".wasp-e2e-release-lock"
    releaseLockSignalFileText = T.pack releaseLockSignalFile
