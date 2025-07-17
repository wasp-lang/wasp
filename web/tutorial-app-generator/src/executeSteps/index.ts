import { chalk } from "zx";

import { ensureDirExists } from "../files";
import { tagChanges } from "../git";
import { log } from "../log";
import { appDir, patchesDir } from "../project";
import { waspDbMigrate } from "../waspCli";
import { type Action } from "./actions";
import { applyPatch, tryToFixPatch } from "./patch";

export async function executeSteps(actions: Action[]): Promise<void> {
  for (const action of actions) {
    const kind = action.kind;
    log("info", `${chalk.bold(`[step ${action.stepName}]`)} ${kind}`);

    await ensureDirExists(patchesDir);

    try {
      switch (kind) {
        case "apply-patch":
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
          await waspDbMigrate(appDir, `step-${action.stepName}`);
          break;
        default:
          kind satisfies never;
      }
    } catch (err) {
      log("error", `Error in step ${action.stepName}:\n\n${err}`);
      process.exit(1);
    }
    await tagChanges(appDir, action.stepName);
  }
}
