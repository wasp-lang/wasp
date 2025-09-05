import { confirm } from "@inquirer/prompts";
import { $ } from "zx";

import { appDir } from "./project";

export async function askToOpenProjectInEditor() {
  const wantsToOpenVSCode = await confirm({
    message: `Do you want to open the app in VS Code?`,
  });
  if (wantsToOpenVSCode) {
    await $`code ${appDir}`;
  }
}
