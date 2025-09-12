import fs from "fs/promises";

import { Command, Option } from "@commander-js/extra-typings";
import { ProcessOutput } from "zx";

import { confirm, select } from "@inquirer/prompts";
import type { Action, ApplyPatchAction } from "../../actions/actions";
import {
  applyPatchForAction,
  askUserToEditAndCreatePatch,
  commitActionChanges,
  createBranchFromActionCommit,
  generatePatchForAction,
} from "../../actions/git";
import { getActionsFromTutorialFiles } from "../../extract-actions";
import {
  mainBranchName,
  moveLastCommitChangesToStaging,
  rebaseBranch,
} from "../../git";
import { log } from "../../log";
import {
  docsTutorialDirPath,
  tutorialAppDirPath,
  type AppDirPath,
} from "../../tutorialApp";
import { generateApp } from "../generate-app";

export const editActionCommand = new Command("edit-action")
  .description("Edit a action in the tutorial app")
  .addOption(new Option("--action-id <id>", "ID of the action to edit"))
  .addOption(
    new Option(
      "--skip-generating-app",
      "Skip generating app before editing action",
    ),
  )
  .action(async ({ actionId, skipGeneratingApp }) => {
    const actions = await getActionsFromTutorialFiles(docsTutorialDirPath);
    log("info", `Found ${actions.length} actions in tutorial files.`);

    const action = await ensureAction({
      actions,
      actionIdOptionValue: actionId,
    });

    if (!skipGeneratingApp) {
      log("info", "Generating app before editing action...");
      await generateApp(actions);
    } else {
      log(
        "info",
        `Skipping app generation, using existing app in ${tutorialAppDirPath}`,
      );
    }

    log("info", `Editing action ${action.displayName}...`);

    await editActionPatch({ appDir: tutorialAppDirPath, action });

    await extractCommitsIntoPatches(actions);

    log("success", `Edit completed for action ${action.displayName}!`);
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
    log("info", `Updating patch for action ${action.displayName}`);
    const patch = await generatePatchForAction({
      appDir: tutorialAppDirPath,
      action,
    });
    await fs.writeFile(action.patchFilePath, patch, "utf-8");
  }
}

async function ensureAction({
  actions,
  actionIdOptionValue,
}: {
  actions: Action[];
  actionIdOptionValue: string | undefined;
}): Promise<ApplyPatchAction> {
  const applyPatchActions = actions.filter(
    (action) => action.kind === "apply-patch",
  );

  if (!actionIdOptionValue) {
    return askUserToSelectAction(applyPatchActions);
  }

  const action = applyPatchActions.find((a) => a.id === actionIdOptionValue);
  if (!action) {
    throw new Error(
      `Apply patch action with ID "${actionIdOptionValue}" not found.`,
    );
  }
  return action;
}

async function askUserToSelectAction(
  actions: ApplyPatchAction[],
): Promise<ApplyPatchAction> {
  const selectedActionId = await select({
    message: "Select a action to edit",
    choices: actions.map((action) => ({
      name: action.displayName,
      value: action.id,
    })),
  });
  return actions.find((a) => a.id === selectedActionId) as ApplyPatchAction;
}
