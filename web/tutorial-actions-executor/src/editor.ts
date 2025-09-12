import { confirm } from "@inquirer/prompts";
import { $ } from "zx";
import { tutorialAppDirPath } from "./tutorialApp";

export async function askToOpenProjectInEditor() {
  const editor = process.env.EDITOR;
  if (!editor) {
    return;
  }
  const wantsToOpenVSCode = await confirm({
    message: `Do you want to open the app in ${editor}?`,
  });
  if (wantsToOpenVSCode) {
    await $`$EDITOR ${tutorialAppDirPath}`;
  }
}
