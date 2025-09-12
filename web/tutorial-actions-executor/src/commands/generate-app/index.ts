import { Command } from "@commander-js/extra-typings";

import type { Action } from "../../actions/actions";
import { getActionsFromTutorialFiles } from "../../extract-actions";
import { log } from "../../log";
import {
  docsTutorialDirPath,
  docsTutorialPatchesPath,
  tutorialAppDirPath,
} from "../../tutorialApp";
import { executeActions } from "./execute-actions";

export const generateAppCommand = new Command("generate-app")
  .description("Generate a new Wasp app based on the tutorial actions")
  .action(async () => {
    const actions = await getActionsFromTutorialFiles(docsTutorialDirPath);
    log("info", `Found ${actions.length} actions in tutorial files.`);

    await generateApp(actions);
  });

export async function generateApp(actions: Action[]): Promise<void> {
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
