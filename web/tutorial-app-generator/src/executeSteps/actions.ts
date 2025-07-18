import path from "path";
import type {
  MarkdownFilePath,
  PatchFilePath,
  StepName,
} from "../brandedTypes";
import { getFileNameWithoutExtension } from "../files";
import { patchesDir } from "../project";

export type ActionCommon = {
  stepName: StepName;
  sourceFilePath: MarkdownFilePath;
};

export type ApplyPatchAction = {
  kind: "apply-patch";
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
    patchFilePath,
  };
}

function getPatchFilePath(action: ActionCommon): PatchFilePath {
  const sourceFileName = getFileNameWithoutExtension(action.sourceFilePath);
  const patchFileName = `${sourceFileName}__${action.stepName}.patch`;
  return path.resolve(patchesDir, patchFileName) as PatchFilePath;
}
