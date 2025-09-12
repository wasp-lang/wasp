import { Command } from "@commander-js/extra-typings";
import { $, fs, spinner } from "zx";

import type { Action } from "../../actions/actions";
import { getActionsFromTutorialFiles } from "../../extract-actions";
import { initGitRepo, mainBranchName } from "../../git";
import { log } from "../../log";
import {
  docsTutorialDirPath,
  docsTutorialPatchesPath,
  tutorialAppDirPath,
  tutorialAppName,
  tutorialAppParentDirPath,
  type AppDirPath,
  type AppName,
  type AppParentDirPath,
} from "../../tutorialApp";
import { waspNew } from "../../waspCli";
import { executeActions } from "./execute-actions";

export const generateAppCommand = new Command("generate-app")
  .description("Generate a new Wasp app based on the tutorial actions")
  .action(async () => {
    const actions = await getActionsFromTutorialFiles(docsTutorialDirPath);
    log("info", `Found ${actions.length} actions in tutorial files.`);

    await generateApp(actions);
  });

export async function generateApp(actions: Action[]): Promise<void> {
  await spinner("Initializing the tutorial app...", () =>
    initApp({
      tutorialAppDirPath,
      tutorialAppParentDirPath,
      tutorialAppName,
      mainBranchName,
    }),
  );
  await executeActions({
    appDir: tutorialAppDirPath,
    patchesDir: docsTutorialPatchesPath,
    actions,
  });
  log(
    "success",
    `Tutorial app has been successfully generated in ${tutorialAppDirPath}`,
  );
}

async function initApp({
  tutorialAppName,
  tutorialAppDirPath,
  tutorialAppParentDirPath,
  mainBranchName,
}: {
  tutorialAppName: AppName;
  tutorialAppParentDirPath: AppParentDirPath;
  tutorialAppDirPath: AppDirPath;
  mainBranchName: string;
}): Promise<void> {
  await fs.ensureDir(tutorialAppParentDirPath);
  await $`rm -rf ${tutorialAppDirPath}`;
  await waspNew({
    appName: tutorialAppName,
    appParentDir: tutorialAppParentDirPath,
  });
  await initGitRepo(tutorialAppDirPath, mainBranchName);
  log("info", `Tutorial app has been initialized in ${tutorialAppDirPath}`);
}
