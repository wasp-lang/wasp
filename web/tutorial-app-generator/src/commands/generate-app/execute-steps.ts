import { chalk, fs } from "zx";

import { type Action } from "../../actions";
import {
  applyPatchForAction,
  commitActionChanges,
  regeneratePatchForAction,
} from "../../actions/git";
import type { AppDirPath, PatchesDirPath } from "../../brandedTypes";
import { log } from "../../log";
import { waspDbMigrate } from "../../waspCli";

export async function executeSteps({
  appDir,
  patchesDir,
  actions,
}: {
  appDir: AppDirPath;
  patchesDir: PatchesDirPath;
  actions: Action[];
}): Promise<void> {
  for (const action of actions) {
    log("info", `${chalk.bold(`[step ${action.id}]`)} ${action.kind}`);

    await fs.ensureDir(patchesDir);

    try {
      switch (action.kind) {
        case "apply-patch":
          try {
            await applyPatchForAction({ appDir, action });
          } catch (err) {
            log(
              "error",
              `Failed to apply patch for step ${action.displayName}:\n${err}`,
            );
            await regeneratePatchForAction({ appDir, action });
            await applyPatchForAction({ appDir, action });
          }
          break;
        case "migrate-db":
          await waspDbMigrate(appDir, action.id);
          break;
        default:
          action satisfies never;
      }
    } catch (err) {
      log("error", `Error in step with ID ${action.id}:\n\n${err}`);
      process.exit(1);
    }
    await commitActionChanges({ appDir, action });
  }
}
