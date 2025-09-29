import path, { basename } from "path";

import { getFileNameWithoutExtension } from "../files";
import type { PatchesDirPath } from "../tutorialApp";
import type {
  ActionId,
  ApplyPatchAction,
  BaseAction,
  InitAppAction,
  MdxFilePath,
  MigrateDbAction,
  PatchFilePath,
} from "./actions";

export function createInitAppAction(
  commonData: BaseAction,
  waspStarterTemplateName: string,
): InitAppAction {
  return {
    ...commonData,
    kind: "INIT_APP",
    waspStarterTemplateName,
  };
}

export function createMigrateDbAction(commonData: BaseAction): MigrateDbAction {
  return {
    ...commonData,
    kind: "MIGRATE_DB",
  };
}

export function createApplyPatchAction(
  commonData: BaseAction,
  patchesDirPath: PatchesDirPath,
): ApplyPatchAction {
  const patchFilename = getPatchFilename(
    commonData.id,
    commonData.tutorialFilePath,
  );
  return {
    ...commonData,
    kind: "APPLY_PATCH",
    displayName: `${basename(commonData.tutorialFilePath)} / ${commonData.id}`,
    patchFilePath: path.resolve(patchesDirPath, patchFilename) as PatchFilePath,
  };
}

function getPatchFilename(id: ActionId, tutorialFilePath: MdxFilePath): string {
  return `${getFileNameWithoutExtension(tutorialFilePath)}__${id}.patch`;
}
