import fs from "fs/promises";
import path from "path";

import { $ } from "zx";

import Enquirer from "enquirer";
import { assertValidPatchFile } from "../diff";
import { log } from "../log";
import { waspDbMigrate } from "../waspCli";

export type ActionCommon = {
  step: string;
  markdownSourceFilePath: string;
};

export type WriteFileAction = {
  kind: "write";
  path: string;
  content: string;
} & ActionCommon;

export type ApplyPatchAction = {
  kind: "diff";
  patchContentPath: string;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "migrate-db";
} & ActionCommon;

export type Action = WriteFileAction | ApplyPatchAction | MigrateDbAction;

export async function writeFile(appDir: string, action: WriteFileAction) {
  const filePath = path.resolve(appDir, action.path);
  await fs.writeFile(filePath, action.content);
  log("info", `Wrote to ${action.path}`);
}

export async function ensurePatchExists(
  appDir: string,
  action: ApplyPatchAction,
) {
  const patchPath = path.resolve(appDir, action.patchContentPath);
  if (!(await fs.stat(patchPath).catch(() => false))) {
    await Enquirer.prompt({
      type: "confirm",
      name: "edit",
      message: `Apply edit for ${action.step} and press Enter`,
      initial: true,
    });
    const patch = await generateGitPatch(appDir);
    await fs.writeFile(action.patchContentPath, patch, "utf-8");
    log("info", `Patch file created: ${action.patchContentPath}`);
    await undoChanges(appDir);
  }
  assertValidPatchFile(action.patchContentPath);
}

function undoChanges(appDir: string) {
  return $`cd ${appDir} && git reset --hard HEAD && git clean -fd`.quiet(true);
}

export async function generateGitPatch(appDir: string): Promise<string> {
  const { stdout: patch } = await $`cd ${appDir} && git diff`.verbose(false);
  return patch;
}

export async function commitStep(appDir: string, action: ActionCommon) {
  await $`cd ${appDir} && git add . && git commit -m "${action.step}" && git tag ${action.step}`;
  log("info", `Committed step ${action.step}`);
}

export async function applyPatch(appDir: string, action: ApplyPatchAction) {
  await $`cd ${appDir} && git apply ${action.patchContentPath} --verbose`.quiet(
    true,
  );
}

export const migrateDb = waspDbMigrate;

export function createApplyPatchAction(
  commonActionData: ActionCommon,
): ApplyPatchAction {
  const patchContentPath = path.resolve(
    "../docs/tutorial",
    "patches",
    `${commonActionData.step}.patch`,
  );

  return {
    ...commonActionData,
    kind: "diff",
    patchContentPath,
  };
}
