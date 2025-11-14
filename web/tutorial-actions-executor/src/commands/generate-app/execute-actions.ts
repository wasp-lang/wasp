import { chalk, fs, spinner } from "zx";

import type { Action } from "../../actions/actions";
import {
  applyPatchForAction,
  commitActionChanges,
  regeneratePatchForAction,
} from "../../actions/git";
import { initWaspAppWithGitRepo } from "../../actions/init";
import { assertUnreachable } from "../../assert";
import { mainBranchName } from "../../git";
import { log } from "../../log";
import type { TutorialApp } from "../../tutorialApp";
import { waspDbMigrate, type WaspCliCommand } from "../../waspCli";

export async function executeActions({
  tutorialApp,
  actions,
  waspCliCommand,
}: {
  tutorialApp: TutorialApp;
  actions: Action[];
  waspCliCommand: WaspCliCommand;
}): Promise<void> {
  for (const action of actions) {
    log("info", `${chalk.bold(`[action ${action.id}]`)} ${action.kind}`);

    await fs.ensureDir(tutorialApp.docsTutorialPatchesPath);

    switch (action.kind) {
      case "INIT_APP":
        await spinner("Initializing the tutorial app...", () =>
          initWaspAppWithGitRepo({
            waspCliCommand,
            appName: tutorialApp.name,
            outputDir: tutorialApp.outputDir,
            appDirPath: tutorialApp.appDirPath,
            mainBranchName,
          }),
        );
        break;
      case "APPLY_PATCH":
        try {
          await applyPatchForAction({ appDir: tutorialApp.appDirPath, action });
        } catch (err) {
          log(
            "error",
            `Failed to apply patch for action ${action.displayName}:\n${err}`,
          );
          await regeneratePatchForAction({
            appDir: tutorialApp.appDirPath,
            action,
          });
          await applyPatchForAction({ appDir: tutorialApp.appDirPath, action });
        }
        break;
      case "MIGRATE_DB":
        await waspDbMigrate({
          waspCliCommand,
          appDir: tutorialApp.appDirPath,
          migrationName: action.id,
        });
        break;
      default:
        assertUnreachable(action, `Unknown action '${action}'`);
    }
    await commitActionChanges({ appDir: tutorialApp.appDirPath, action });
  }
}
