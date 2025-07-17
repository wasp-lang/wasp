import fs from "fs/promises";
import path from "path";

import Enquirer from "enquirer";
import parseGitDiff from "parse-git-diff";
import { $ } from "zx";

import type { AppDirPath, PatchContentPath } from "../brandedTypes";
import { doesFileExist } from "../files";
import { generatePatchFromChanges } from "../git";
import { log } from "../log";
import type { ApplyPatchAction } from "./actions";

export async function applyPatch(
  appDir: AppDirPath,
  patchContentPath: PatchContentPath,
) {
  await $({ cwd: appDir })`git apply ${patchContentPath} --verbose`.quiet(true);
}

export async function tryToFixPatch(
  appDir: AppDirPath,
  action: ApplyPatchAction,
): Promise<void> {
  log("info", `Trying to fix patch for step: ${action.stepName}`);

  const patchPath = path.resolve(appDir, action.patchContentPath);
  if (await doesFileExist(patchPath)) {
    log("info", `Removing existing patch file: ${patchPath}`);
    await fs.unlink(patchPath);
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
  await Enquirer.prompt({
    type: "confirm",
    name: "edit",
    message: `Apply edit for ${action.stepName} and press Enter`,
    initial: true,
  });
  const patch = await generatePatchFromChanges(appDir);
  assertValidPatch(patch);
  await fs.writeFile(action.patchContentPath, patch, "utf-8");
  log("info", `Patch file created: ${action.patchContentPath}`);
}

export async function assertValidPatch(patch: string): Promise<void> {
  const parsedPatch = parseGitDiff(patch);

  if (parsedPatch.files.length === 0 || parsedPatch.files[0] === undefined) {
    throw new Error("Invalid patch: no changes found");
  }
}
