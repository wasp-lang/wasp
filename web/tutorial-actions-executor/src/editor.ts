import { confirm } from "@inquirer/prompts";
import { $ } from "zx";

import type { AppDirPath } from "./tutorialApp";

export async function askToOpenTutorialAppInEditor(
  appDirPath: AppDirPath,
): Promise<void> {
  const editor = process.env.EDITOR;
  if (!editor) {
    return;
  }
  const wantsToOpenVSCode = await confirm({
    message: `Do you want to open the tutorial app in ${editor}? (Found in $EDITOR env variable)`,
  });
  if (wantsToOpenVSCode) {
    await $`$EDITOR ${appDirPath}`;
  }
}
