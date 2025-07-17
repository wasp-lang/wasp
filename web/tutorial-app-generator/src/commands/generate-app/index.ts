import { $ } from "zx";

import { Command } from "@commander-js/extra-typings";
import { executeSteps } from "../../executeSteps";
import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { log } from "../../log";
import { appDir } from "../../paths";
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
  await $`cd ${appDir} && git init && git add . && git commit -m "Initial commit"`;
}
