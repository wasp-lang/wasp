import fs from "fs/promises";
import path from "path";

import Enquirer from "enquirer";
import { $ } from "zx";

import parseGitDiff from "parse-git-diff";

import { generatePatchFromChanges } from "../git";
import { log } from "../log";
import type { ApplyPatchAction } from "./actions";

export async function applyPatch(appDir: string, patchContentPath: string) {
  await $`cd ${appDir} && git apply ${patchContentPath} --verbose`.quiet(true);
}

export async function tryToFixPatch(
  appDir: string,
  action: ApplyPatchAction,
): Promise<void> {
  log("info", `Trying to fix patch for step: ${action.stepName}`);

  const patchPath = path.resolve(appDir, action.patchContentPath);
  if (await fs.stat(patchPath).catch(() => false)) {
    log("info", `Removing existing patch file: ${patchPath}`);
    await fs.unlink(patchPath);
  }

  await createPatchForStep(appDir, action);
}

export async function createPatchForStep(
  appDir: string,
  action: ApplyPatchAction,
) {
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
