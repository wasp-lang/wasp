import { chalk, fs } from "zx";

import type { AppDirPath, PatchesDirPath } from "../brandedTypes";
import { tagAllChanges } from "../git";
import { log } from "../log";
import { waspDbMigrate } from "../waspCli";
import { type Action } from "./actions";
import { applyPatch, tryToFixPatch } from "./patch";

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
            await applyPatch({ appDir, action });
          } catch (err) {
            log(
              "error",
              `Failed to apply patch for step ${action.displayName}:\n${err}`,
            );
            await tryToFixPatch({ appDir, action });
            await applyPatch({ appDir, action });
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
    await tagAllChanges(appDir, action.id);
  }
}
