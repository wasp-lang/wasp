import { chalk, fs } from "zx";

import type { Action } from "../../actions/actions";
import {
  applyPatchForAction,
  commitActionChanges,
  regeneratePatchForAction,
} from "../../actions/git";
import { log } from "../../log";
import type { AppDirPath, PatchesDirPath } from "../../tutorialApp";
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
          break;
        case "MIGRATE_DB":
          await waspDbMigrate(appDir, action.id);
          break;
        default:
          action satisfies never;
      }
    } catch (err) {
      log("error", `Error in action with ID ${action.id}:\n\n${err}`);
      process.exit(1);
    }
    await commitActionChanges({ appDir, action });
  }
}
