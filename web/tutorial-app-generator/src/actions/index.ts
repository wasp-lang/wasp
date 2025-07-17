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

export async function ensurePatchExists(
  appDir: string,
  action: ApplyPatchAction,
) {
  const patchPath = path.resolve(appDir, action.patchContentPath);
  if (!(await fs.stat(patchPath).catch(() => false))) {
    await Enquirer.prompt({
      type: "confirm",
      name: "edit",
      message: `Apply edit for ${action.stepName} and press Enter`,
      initial: true,
    });
    await commitStep(appDir, action.stepName);
    const patch = await generateGitPatch(appDir, action.stepName);
    await fs.writeFile(action.patchContentPath, patch, "utf-8");
    log("info", `Patch file created: ${action.patchContentPath}`);
  }
  assertValidPatchFile(action.patchContentPath);
}

function undoChanges(appDir: string) {
  return $`cd ${appDir} && git reset --hard HEAD && git clean -fd`.quiet(true);
}

export async function generateGitPatch(
  appDir: string,
  stepName: string,
): Promise<string> {
  const { stdout: patch } =
    await $`cd ${appDir} && git show --format= ${stepName}`.verbose(false);
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
