import fs from "fs/promises";

import { Command, Option } from "@commander-js/extra-typings";
import { ProcessOutput } from "zx";

import { confirm, select } from "@inquirer/prompts";
import type { Action, ApplyPatchAction } from "../../actions";
import {
  applyPatchForAction,
  askUserToEditAndCreatePatch,
  commitActionChanges,
  createBranchFromActionCommit,
  generatePatchForAction,
} from "../../actions/git";
import type { AppDirPath } from "../../brandedTypes";
import { getActionsFromTutorialFiles } from "../../extractSteps";
import { moveLastCommitChangesToStaging, rebaseBranch } from "../../git";
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

    await editActionPatch({ appDir, action });

    await extractCommitsIntoPatches(actions);

    log("success", `Edit completed for step ${action.displayName}!`);
  });

async function editActionPatch({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}): Promise<void> {
  const fixesBranchName = "fixes";

  await createBranchFromActionCommit({
    appDir,
    branchName: fixesBranchName,
    action,
  });

  await moveLastCommitChangesToStaging(appDir);
  await askUserToEditAndCreatePatch({ appDir, action });
  await applyPatchForAction({ appDir, action });
  await commitActionChanges({ appDir, action });

  try {
    await rebaseBranch({
      gitRepoDir: appDir,
      branchName: fixesBranchName,
      baseBranchName: mainBranchName,
    });
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
    const patch = await generatePatchForAction({ appDir, action });
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
