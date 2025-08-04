import { Command } from "@commander-js/extra-typings";
import { $ } from "zx";

import type { AppDirPath, AppName, AppParentDirPath } from "../../brandedTypes";
import { executeSteps } from "../../executeSteps";
import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { initGitRepo } from "../../git";
import { log } from "../../log";
import {
  appDir,
  appName,
  appParentDir,
  mainBranchName,
  patchesDir,
  tutorialDir,
} from "../../project";
import { waspNew } from "../../waspCli";

export const generateAppCommand = new Command("generate-app")
  .description("Generate a new Wasp app based on the tutorial steps")
  .action(async () => {
    const actions = await getActionsFromTutorialFiles(tutorialDir);
    log("success", `Found ${actions.length} actions in tutorial files.`);

    await generateApp(actions);
  });

export async function generateApp(actions: Action[]): Promise<void> {
  await prepareApp({ appDir, appParentDir, appName, mainBranchName });
  await executeSteps({ appDir, patchesDir, actions });
  log("success", `Tutorial app has been successfully generated in ${appDir}`);
}

async function prepareApp({
  appName,
  appParentDir,
  appDir,
  mainBranchName,
}: {
  appName: AppName;
  appParentDir: AppParentDirPath;
  appDir: AppDirPath;
  mainBranchName: string;
}): Promise<void> {
  await $`rm -rf ${appDir}`;
  await waspNew({ appName, appParentDir });
  await initGitRepo(appDir, mainBranchName);
  log("info", `Tutorial app has been initialized in ${appDir}`);
}
