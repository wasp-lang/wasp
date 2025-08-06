import { confirm } from "@inquirer/prompts";
import parseGitDiff from "parse-git-diff";
import { $, fs } from "zx";

import type { AppDirPath } from "../brandedTypes";
import { generatePatchFromAllChanges } from "../git";
import { log } from "../log";
import type { ApplyPatchAction } from "./actions";

export async function applyPatch({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}) {
  await $({ cwd: appDir })`git apply ${action.patchFilePath} --verbose`.quiet(
    true,
  );
}

export async function tryToFixPatch({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}): Promise<void> {
  log("info", `Trying to fix patch for step: ${action.displayName}`);

  if (await fs.pathExists(action.patchFilePath)) {
    log("info", `Removing existing patch file: ${action.patchFilePath}`);
    await fs.unlink(action.patchFilePath);
  }

  await createPatchForStep({ appDir, action });
}

export async function createPatchForStep({
  appDir,
  action,
}: {
  appDir: AppDirPath;
  action: ApplyPatchAction;
}) {
  const wantsToOpenVSCode = await confirm({
    message: `Do you want to open the app in VS Code to make changes for step "${action.displayName}"?`,
  });
  if (wantsToOpenVSCode) {
    await $`code ${appDir}`;
  }
  await confirm({
    message: `Do the step ${action.displayName} and press Enter`,
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
