import { Command } from "@commander-js/extra-typings";
import { $ } from "zx";

import type { AppDirPath, AppName } from "../../brandedTypes";
import { executeSteps } from "../../executeSteps";
import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { initGitRepo } from "../../git";
import { log } from "../../log";
import {
  appDir,
  appName,
  mainBranchName,
  patchesDir,
  tutorialDir,
} from "../../project";
import { waspNew } from "../../waspCli";

export const generateAppCommand = new Command("generate-app")
  .description("Generate a new Wasp app based on the tutorial steps")
  .action(async () => {
    const actions: Action[] = await getActionsFromTutorialFiles(tutorialDir);

    await prepareApp({ appDir, appName, mainBranchName });
    await executeSteps({ appDir, patchesDir, actions });
    log("success", "Tutorial app has been successfully generated!");
  });

async function prepareApp({
  appName,
  appDir,
  mainBranchName,
}: {
  appDir: AppDirPath;
  appName: AppName;
  mainBranchName: string;
}): Promise<void> {
  await $`rm -rf ${appDir}`;
  await waspNew(appName);
  await initGitRepo(appDir, mainBranchName);
  log("info", "Tutorial app directory has been initialized");
}
