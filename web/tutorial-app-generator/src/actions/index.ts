import fs from "fs/promises";
import path from "path";

import { $ } from "zx";

import Enquirer from "enquirer";
import { assertValidPatchFile } from "../diff";
import { log } from "../log";
import { waspDbMigrate } from "../waspCli";

export type ActionCommon = {
  stepName: string;
  markdownSourceFilePath: string;
};

export type ApplyPatchAction = {
  kind: "diff";
  patchContentPath: string;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "migrate-db";
} & ActionCommon;

export type Action = ApplyPatchAction | MigrateDbAction;

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
  await fs.writeFile(action.patchContentPath, patch, "utf-8");
  log("info", `Patch file created: ${action.patchContentPath}`);

  assertValidPatchFile(action.patchContentPath);
}

export async function generatePatchFromChanges(
  appDir: string,
): Promise<string> {
  const temporaryTagName = "temporary-patch-tag";
  const { stdout: patch } = await $`cd ${appDir} &&
    git add . &&
    git commit -m "${temporaryTagName}" &&
    git show --format= ${temporaryTagName}
    git reset --hard HEAD~1
    `.verbose(false);
  return patch;
}

export async function commitStep(appDir: string, stepName: string) {
  await $`cd ${appDir} && git add . && git commit -m "${stepName}" && git tag ${stepName}`;
  log("info", `Committed step ${stepName}`);
}

export async function applyPatch(appDir: string, patchContentPath: string) {
  await $`cd ${appDir} && git apply ${patchContentPath} --verbose`.quiet(true);
}

export const migrateDb = waspDbMigrate;

export function createApplyPatchAction(
  commonActionData: ActionCommon,
): ApplyPatchAction {
  const patchContentPath = path.resolve(
    "../docs/tutorial",
    "patches",
    `${commonActionData.stepName}.patch`,
  );

  return {
    ...commonActionData,
    kind: "diff",
    patchContentPath,
  };
}
