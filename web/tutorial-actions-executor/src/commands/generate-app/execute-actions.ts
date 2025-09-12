import { chalk, fs, spinner } from "zx";

import type { Action } from "../../actions/actions";
import {
  applyPatchForAction,
  commitActionChanges,
  regeneratePatchForAction,
} from "../../actions/git";
import { initApp } from "../../actions/init";
import { mainBranchName } from "../../git";
import { log } from "../../log";
import {
  tutorialAppDirPath,
  tutorialAppName,
  tutorialAppParentDirPath,
  type AppDirPath,
  type PatchesDirPath,
} from "../../tutorialApp";
import { waspDbMigrate } from "../../waspCli";

export async function executeActions({
  appDir,
  patchesDir,
  actions,
}: {
  appDir: AppDirPath;
  patchesDir: PatchesDirPath;
  actions: Action[];
}): Promise<void> {
  for (const action of actions) {
    log("info", `${chalk.bold(`[action ${action.id}]`)} ${action.kind}`);

    await fs.ensureDir(patchesDir);

    try {
      switch (action.kind) {
        case "INIT_APP":
          await spinner("Initializing the tutorial app...", () =>
            initApp({
              tutorialAppDirPath,
              tutorialAppParentDirPath,
              tutorialAppName,
              mainBranchName,
            }),
          );
          break;
        case "APPLY_PATCH":
          try {
            await applyPatchForAction({ appDir, action });
          } catch (err) {
            log(
              "error",
              `Failed to apply patch for action ${action.displayName}:\n${err}`,
            );
            await regeneratePatchForAction({ appDir, action });
            await applyPatchForAction({ appDir, action });
          }
          await commitActionChanges({ appDir, action });
          break;
        case "MIGRATE_DB":
          await waspDbMigrate(appDir, action.id);
          await commitActionChanges({ appDir, action });
          break;
        default:
          action satisfies never;
      }
    } catch (err) {
      log("error", `Error in action with ID ${action.id}:\n\n${err}`);
      process.exit(1);
    }
  }
}
