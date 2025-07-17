import { chalk } from "zx";

import {
  applyPatch,
  commitStep,
  migrateDb,
  tryToFixPatch,
  type Action,
} from "../actions";
import { log } from "../log";
import { appDir, ensureDirExists, patchesDir } from "../paths";

export async function executeSteps(actions: Action[]): Promise<void> {
  for (const action of actions) {
    const kind = action.kind;
    log("info", `${chalk.bold(`[step ${action.stepName}]`)} ${kind}`);

    // Prepare the patches directory
    await ensureDirExists(patchesDir);

    try {
      switch (kind) {
        case "diff":
          try {
            await applyPatch(appDir, action.patchContentPath);
          } catch (err) {
            log(
              "error",
              `Failed to apply patch for step ${action.stepName}:\n${err}`,
            );
            await tryToFixPatch(appDir, action);
          }
          break;
        case "migrate-db":
          await migrateDb(appDir, `step-${action.stepName}`);
          break;
        default:
          kind satisfies never;
      }
    } catch (err) {
      log("error", `Error in step ${action.stepName}:\n\n${err}`);
      process.exit(1);
    }
    await commitStep(appDir, action.stepName);
  }
}
