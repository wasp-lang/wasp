import { Command } from "@commander-js/extra-typings";
import { $ } from "zx";

import type { AppDirPath, AppName } from "../../brandedTypes";
import { executeSteps } from "../../executeSteps";
import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { log } from "../../log";
import { appDir, appName, mainBranchName, patchesDir } from "../../project";
import { waspNew } from "../../waspCli";

export const generateAppCommand = new Command("generate-app")
  .description("Generate a new Wasp app based on the tutorial steps")
  .action(async () => {
    const actions: Action[] = await getActionsFromTutorialFiles();

    await prepareApp({ appDir, appName });
    await executeSteps({ appDir, patchesDir, actions });
    log("success", "Tutorial app has been successfully generated!");
  });

async function prepareApp({
  appName,
  appDir,
}: {
  appDir: AppDirPath;
  appName: AppName;
}): Promise<void> {
  await $`rm -rf ${appDir}`;
  await waspNew(appName);
  // Git needs to be initialized for patches to work
  await $({ cwd: appDir })`git init`.quiet(true);
  await $({ cwd: appDir })`git branch -m ${mainBranchName}`;
  await $({ cwd: appDir })`git add .`;
  await $({ cwd: appDir })`git commit -m "Initial commit"`;
  log("info", "Tutorial app directory has been initialized");
}
