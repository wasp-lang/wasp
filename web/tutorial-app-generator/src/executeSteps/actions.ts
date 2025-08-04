import path, { basename } from "path";
import type { MarkdownFilePath, PatchFilePath, StepId } from "../brandedTypes";
import { getFileNameWithoutExtension } from "../files";
import { patchesDir } from "../project";

export type ActionCommon = {
  id: StepId;
  sourceFilePath: MarkdownFilePath;
};

export type ApplyPatchAction = {
  kind: "apply-patch";
  displayName: string;
  patchFilePath: PatchFilePath;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "migrate-db";
} & ActionCommon;

export type Action = ApplyPatchAction | MigrateDbAction;

export function createMigrateDbAction(
  commonData: ActionCommon,
): MigrateDbAction {
  return {
    ...commonData,
    kind: "migrate-db",
  };
}

export function createApplyPatchAction(
  commonData: ActionCommon,
): ApplyPatchAction {
  const patchFilePath = getPatchFilePath(commonData);
  return {
    ...commonData,
    kind: "apply-patch",
    displayName: `${basename(commonData.sourceFilePath)} / ${commonData.id}`,
    patchFilePath,
  };
}

function getPatchFilePath(action: ActionCommon): PatchFilePath {
  const sourceFileName = getFileNameWithoutExtension(action.sourceFilePath);
  const patchFileName = `${sourceFileName}__${action.id}.patch`;
  return path.resolve(patchesDir, patchFileName) as PatchFilePath;
}
