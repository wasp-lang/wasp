import { confirm } from "@inquirer/prompts";
import parseGitDiff from "parse-git-diff";
import { fs } from "zx";

import { askToOpenTutorialAppInEditor } from "../editor";
import {
  applyPatch,
  commitAllChanges,
  createBranchFromRevision,
  findCommitSHAForExactMessage,
  generatePatchFromAllChanges,
  generatePatchFromRevision,
} from "../git";
import { log } from "../log";
import type { AppDirPath } from "../tutorialApp";
import type { Action, ApplyPatchAction } from "./actions";

export function commitActionChanges({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: Action;
}) {
  return commitAllChanges(appDir, action.id);
}

export function getActionCommitSHA({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: Action;
}): Promise<string> {
  return findCommitSHAForExactMessage(appDir, action.id);
}

export async function generatePatchForAction({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}): Promise<string> {
  const actionCommitSha = await getActionCommitSHA({ appDir, action });
  return generatePatchFromRevision(appDir, actionCommitSha);
}

export async function applyPatchForAction({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}): Promise<void> {
  await applyPatch(appDir, action.patchFilePath);
}

export async function regeneratePatchForAction({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}): Promise<void> {
  log("info", `Trying to fix patch for action: ${action.displayName}`);

  if (await fs.pathExists(action.patchFilePath)) {
    log("info", `Removing existing patch file: ${action.patchFilePath}`);
    await fs.unlink(action.patchFilePath);
  }

  await askUserToEditAndCreatePatch({ appDir, action });
}

export async function askUserToEditAndCreatePatch({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}) {
  await askToOpenTutorialAppInEditor(appDir);
  await confirm({
    message: `Update the app according to action ${action.displayName} and press Enter`,
  });

  const patch = await generatePatchFromAllChanges(appDir);
  assertValidPatch(patch);
  await fs.writeFile(action.patchFilePath, patch, "utf-8");

  log("success", `Patch file created: ${action.patchFilePath}`);
}

export async function assertValidPatch(patch: string): Promise<void> {
  const parsedPatch = parseGitDiff(patch);

  if (parsedPatch.files.length === 0 || parsedPatch.files[0] === undefined) {
    throw new Error("Invalid patch: no changes found");
  }
}

export async function createBranchFromActionCommit({
  appDir,
  branchName,
  action,
}: {
  appDir: AppDirPath;
  branchName: string;
  action: ApplyPatchAction;
}): Promise<void> {
  const actionCommitSha = await getActionCommitSHA({ appDir, action });
  await createBranchFromRevision({
    gitRepoDirPath: appDir,
    branchName,
    revision: actionCommitSha,
  });
}
