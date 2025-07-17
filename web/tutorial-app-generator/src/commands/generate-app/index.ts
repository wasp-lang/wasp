import { Command } from "@commander-js/extra-typings";
import { $ } from "zx";

import { executeSteps } from "../../executeSteps";
import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { log } from "../../log";
import { appDir, mainBranchName } from "../../project";
import { waspNew } from "../../waspCli";

export const generateAppCommand = new Command("generate-app")
  .description("Generate a new Wasp app based on the tutorial steps")
  .action(async () => {
    const actions: Action[] = await getActionsFromTutorialFiles();

    await prepareApp();
    await executeSteps(actions);
    log("success", "Tutorial app has been successfully generated!");
  });

async function prepareApp() {
  await $`rm -rf ${appDir}`;
  await waspNew(appDir);
  // Git needs to be initialized for patches to work
  await $({ cwd: appDir })`git init`.quiet(true);
  await $({ cwd: appDir })`git branch -m ${mainBranchName}`;
  await $({ cwd: appDir })`git add .`;
  await $({ cwd: appDir })`git commit -m "Initial commit"`;
  log("info", "Tutorial app directory has been initialized");
}
