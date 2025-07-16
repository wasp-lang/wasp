import fs from "fs/promises";
import path from "path";

import { $ } from "zx";

import { log } from "../log";
import { waspDbMigrate } from "../waspCli";

export type ActionCommon = {
  step: number;
  markdownSourceFilePath: string;
};

export type WriteFileAction = {
  kind: "write";
  path: string;
  content: string;
} & ActionCommon;

export type ApplyPatchAction = {
  kind: "diff";
  targetFilePath: string;
  patchContentPath: string;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "migrate-db";
} & ActionCommon;

export type Action = WriteFileAction | ApplyPatchAction | MigrateDbAction;

export async function writeFile(appDir: string, file: WriteFileAction) {
  const filePath = path.resolve(appDir, file.path);
  await fs.writeFile(filePath, file.content);
  log("info", `Wrote to ${file.path}`);
}

export async function applyPatch(appDir: string, patch: ApplyPatchAction) {
  // const patchPath = path.resolve(patchesDir, `step-${patch.step}.patch`)
  // await fs.writeFile(patchPath, patch.patch)
  await $`cd ${appDir} && git apply ${patch.patchContentPath} --verbose`.quiet(
    true,
  );
  log("info", `Applied patch to ${patch.targetFilePath}`);
}

export const migrateDb = waspDbMigrate;

export { createApplyPatchAction } from "./diff";
