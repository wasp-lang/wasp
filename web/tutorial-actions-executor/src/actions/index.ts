import path, { basename } from "path";
import { getFileNameWithoutExtension } from "../files";
import { docsTutorialPatchesPath } from "../tutorialApp";
import type {
  ActionCommon,
  ApplyPatchAction,
  MigrateDbAction,
  PatchFilePath,
} from "./actions";

export function createMigrateDbAction(
  commonData: ActionCommon,
): MigrateDbAction {
  return {
    ...commonData,
    kind: "MIGRATE_DB",
  };
}

export function createApplyPatchAction(
  commonData: ActionCommon,
): ApplyPatchAction {
  const patchFilePath = getPatchFilePath(commonData);
  return {
    ...commonData,
    kind: "APPLY_PATCH",
    displayName: `${basename(commonData.tutorialFilePath)} / ${commonData.id}`,
    patchFilePath,
  };
}

function getPatchFilePath(action: ActionCommon): PatchFilePath {
  const sourceFileName = getFileNameWithoutExtension(action.tutorialFilePath);
  const patchFileName = `${sourceFileName}__${action.id}.patch`;
  return path.resolve(docsTutorialPatchesPath, patchFileName) as PatchFilePath;
}
