import fs from "fs/promises";

import { Command, Option } from "@commander-js/extra-typings";
import { $, ProcessOutput } from "zx";

import { confirm, select } from "@inquirer/prompts";
import type { AppDirPath } from "../../brandedTypes";
import type { Action, ApplyPatchAction } from "../../executeSteps/actions";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { generatePatchFromRevision } from "../../git";
import { log } from "../../log";
import { appDir, mainBranchName, tutorialDir } from "../../project";
import { generateApp } from "../generate-app";

export const editStepCommand = new Command("edit-step")
  .description("Edit a step in the tutorial app")
  .addOption(new Option("--step-id <id>", "Name of the step to edit"))
  .addOption(
    new Option(
      "--skip-generating-app",
      "Skip generating app before editing step",
    ),
  )
  .action(async ({ stepId, skipGeneratingApp }) => {
    const actions = await getActionsFromTutorialFiles(tutorialDir);
    log("info", `Found ${actions.length} actions in tutorial files.`);

    const action = await ensureAction({
      actions,
      stepIdOptionValue: stepId,
    });

    if (!skipGeneratingApp) {
      log("info", "Generating app before editing step...");
      await generateApp(actions);
    } else {
      log("info", `Skipping app generation, using existing app in ${appDir}`);
    }

    log("info", `Editing step ${action.displayName}...`);

    await editStepPatch({ appDir, action });

    await extractCommitsIntoPatches(actions);

    log("success", `Edit completed for step ${action.displayName}!`);
  });

async function editStepPatch({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}): Promise<void> {
  await $({ cwd: appDir })`git switch ${mainBranchName}`;

  const fixesBranchName = "fixes";
  await $({
    cwd: appDir,
  })`git switch --force-create ${fixesBranchName} ${action.id}`;

  await confirm({
    message: `Apply edit for step "${action.displayName}" and press Enter`,
  });

  await $({ cwd: appDir })`git add .`;
  await $({ cwd: appDir })`git commit --amend --no-edit`;
  await $({ cwd: appDir })`git tag -f ${action.id}`;
  await $({ cwd: appDir })`git switch ${mainBranchName}`;
  try {
    await $({ cwd: appDir, throw: false })`git rebase ${fixesBranchName}`;
  } catch (error: unknown) {
    if (
      error instanceof ProcessOutput &&
      error.stderr.includes("git rebase --continue")
    ) {
      await confirm({
        message: `Resolve rebase issues and press Enter to continue`,
      });
    } else {
      throw error;
    }
  }
}

async function extractCommitsIntoPatches(actions: Action[]): Promise<void> {
  const applyPatchActions = actions.filter(
    (action) => action.kind === "apply-patch",
  );

  for (const action of applyPatchActions) {
    log("info", `Updating patch for step ${action.displayName}`);
    const patch = await generatePatchFromRevision(appDir, action.id);
    await fs.writeFile(action.patchFilePath, patch, "utf-8");
  }
}

async function ensureAction({
  actions,
  stepIdOptionValue,
}: {
  actions: Action[];
  stepIdOptionValue: string | undefined;
}): Promise<ApplyPatchAction> {
  const applyPatchActions = actions.filter(
    (action) => action.kind === "apply-patch",
  );

  if (!stepIdOptionValue) {
    return askUserToSelectAction(applyPatchActions);
  }

  const action = applyPatchActions.find((a) => a.id === stepIdOptionValue);
  if (!action) {
    throw new Error(
      `Apply patch step with ID "${stepIdOptionValue}" not found.`,
    );
  }
  return action;
}

async function askUserToSelectAction(
  actions: ApplyPatchAction[],
): Promise<ApplyPatchAction> {
  const selectedStepId = await select({
    message: "Select a step to edit",
    choices: actions.map((action) => ({
      name: action.displayName,
      value: action.id,
    })),
  });
  return actions.find((a) => a.id === selectedStepId) as ApplyPatchAction;
}
