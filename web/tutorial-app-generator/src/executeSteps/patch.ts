import fs from "fs/promises";

import Enquirer from "enquirer";
import parseGitDiff from "parse-git-diff";
import { $ } from "zx";

import type { AppDirPath } from "../brandedTypes";
import { doesFileExist } from "../files";
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
  log("info", `Trying to fix patch for step: ${action.stepName}`);

  if (await doesFileExist(action.patchFilePath)) {
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
  await Enquirer.prompt({
    type: "confirm",
    name: "edit",
    message: `Apply edit for ${action.stepName} and press Enter`,
    initial: true,
  });

  const patch = await generatePatchFromAllChanges(appDir);
  assertValidPatch(patch);
  await fs.writeFile(action.patchFilePath, patch, "utf-8");

  log("info", `Patch file created: ${action.patchFilePath}`);
}

export async function assertValidPatch(patch: string): Promise<void> {
  const parsedPatch = parseGitDiff(patch);

  if (parsedPatch.files.length === 0 || parsedPatch.files[0] === undefined) {
    throw new Error("Invalid patch: no changes found");
  }
}
