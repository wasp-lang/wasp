import { chalk } from "zx";
import {
  applyPatch,
  commitStep,
  ensurePatchExists,
  migrateDb,
  writeFile,
  type Action,
} from "../actions";
import { log } from "../log";
import { appDir, ensureDirExists, patchesDir } from "../paths";

export async function executeSteps(
  actions: Action[],
  {
    untilStep,
  }: {
    untilStep?: Action;
  },
): Promise<void> {
  for (const action of actions) {
    if (untilStep && action.step === untilStep.step) {
      log("info", `Stopping before step ${action.step}`);
      process.exit(0);
    }

    const kind = action.kind;
    log("info", `${chalk.bold(`[step ${action.step}]`)} ${kind}`);

    // Prepare the patches directory
    await ensureDirExists(patchesDir);

    try {
      switch (kind) {
        case "diff":
          await ensurePatchExists(appDir, action);
          await applyPatch(appDir, action);
          break;
        case "write":
          await writeFile(appDir, action);
          break;
        case "migrate-db":
          await migrateDb(appDir, `step-${action.step}`);
          break;
        default:
          kind satisfies never;
      }
    } catch (err) {
      log("error", `Error in step ${action.step}:\n\n${err}`);
      process.exit(1);
    }
    await commitStep(appDir, action);
  }
}
