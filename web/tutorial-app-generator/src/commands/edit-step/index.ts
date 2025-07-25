import fs from "fs/promises";

import { Command, Option } from "@commander-js/extra-typings";
import { $ } from "zx";

import Enquirer from "enquirer";
import type { AppDirPath } from "../../brandedTypes";
import type { Action, ApplyPatchAction } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { generatePatchFromRevision } from "../../git";
import { log } from "../../log";
import { appDir, mainBranchName, tutorialDir } from "../../project";

export const editStepCommand = new Command("edit-step")
  .description("Edit a step in the tutorial app")
  .addOption(new Option("--step-name <stepName>", "Name of the step to edit"))
  .action(async ({ stepName }) => {
    const actions: Action[] = await getActionsFromTutorialFiles(tutorialDir);

    const action = actions.find((a) => a.stepName === stepName);

    if (!action) {
      throw new Error(`Step with name "${stepName}" not found.`);
    }

    if (action.kind !== "apply-patch") {
      throw new Error(`Step "${stepName}" is not an editable step.`);
    }

    await editStepPatch({ appDir, action });

    await extractCommitsIntoPatches(actions);

    log("success", `Edit completed for step ${action.stepName}!`);
  });

async function editStepPatch({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}): Promise<void> {
  await $({ cwd: appDir })`git switch ${mainBranchName}`.quiet(true);

  const fixesBranchName = "fixes";
  await $({
    cwd: appDir,
    quiet: true,
  })`git switch --force-create ${fixesBranchName} ${action.stepName}`;

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
}

async function extractCommitsIntoPatches(actions: Action[]): Promise<void> {
  const applyPatchActions = actions.filter(
    (action) => action.kind === "apply-patch",
  );

  for (const action of applyPatchActions) {
    log("info", `Updating patch for step ${action.stepName}`);
    const patch = await generatePatchFromRevision(appDir, action.stepName);
    await fs.writeFile(action.patchFilePath, patch, "utf-8");
  }
}
