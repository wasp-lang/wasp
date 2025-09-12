import path, { basename } from "path";
import { getFileNameWithoutExtension } from "../files";
import type { PatchesDirPath } from "../tutorialApp";
import type {
  ActionCommon,
  ApplyPatchAction,
  InitAppAction,
  MigrateDbAction,
  PatchFilePath,
} from "./actions";

export function createInitAppAction(
  commonData: ActionCommon,
  waspStarterTemplateName: string,
): InitAppAction {
  return {
    ...commonData,
    kind: "INIT_APP",
    waspStarterTemplateName,
  };
}

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
  docsTutorialPatchesPath: PatchesDirPath,
): ApplyPatchAction {
  return {
    ...commonData,
    kind: "APPLY_PATCH",
    displayName: `${basename(commonData.tutorialFilePath)} / ${commonData.id}`,
    patchFilePath: getPatchFilePath(commonData, docsTutorialPatchesPath),
  };
}

function getPatchFilePath(
  action: ActionCommon,
  docsTutorialPatchesPath: PatchesDirPath,
): PatchFilePath {
  const sourceFileName = getFileNameWithoutExtension(action.tutorialFilePath);
  const patchFileName = `${sourceFileName}__${action.id}.patch`;
  return path.resolve(docsTutorialPatchesPath, patchFileName) as PatchFilePath;
}
