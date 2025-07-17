import fs from "fs/promises";

import { Command, Option } from "@commander-js/extra-typings";
import { $ } from "zx";

import Enquirer from "enquirer";
import type { Action } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { getCommitPatch } from "../../git";
import { log } from "../../log";
import { appDir, mainBranchName } from "../../project";

export const editStepCommand = new Command("edit-step")
  .description("Edit a step in the tutorial app")
  .addOption(new Option("--step-name <stepName>", "Name of the step to edit"))
  .action(async ({ stepName }) => {
    const actions: Action[] = await getActionsFromTutorialFiles();

    const action = actions.find((a) => a.stepName === stepName);

    if (!action) {
      throw new Error(`Step with name "${stepName}" not found.`);
    }

    const fixesBranchName = "fixes";
    await $({
      cwd: appDir,
    })`git switch -c ${fixesBranchName} ${action.stepName}`;

    await Enquirer.prompt({
      type: "confirm",
      name: "edit",
      message: `Apply edit for step "${action.stepName}" and press Enter`,
      initial: true,
    });

    await $({ cwd: appDir })`git add .`;
    await $({ cwd: appDir })`git commit --amend --no-edit`;
    await $({ cwd: appDir })`git tag -f ${action.stepName}`;
    await $({ cwd: appDir })`git switch ${mainBranchName}`;
    await $({ cwd: appDir, throw: false })`git rebase ${fixesBranchName}`;

    await Enquirer.prompt({
      type: "confirm",
      name: "issues",
      message: `If there are any rebase issues, resolve them and press Enter to continue`,
      initial: true,
    });

    await extractCommitsIntoPatches(actions);

    log("success", `Edit completed for step ${action.stepName}!`);
  });

async function extractCommitsIntoPatches(actions: Action[]): Promise<void> {
  for (const action of actions) {
    if (action.kind !== "apply-patch") {
      continue;
    }

    log("info", `Updating patch for step ${action.stepName}`);
    const patch = await getCommitPatch(appDir, action.stepName);
    await fs.writeFile(action.patchContentPath, patch, "utf-8");
  }
}
