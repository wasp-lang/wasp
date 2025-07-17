import path from "path";
import type { PatchContentPath } from "../brandedTypes";

export type ActionCommon = {
  stepName: string;
  markdownSourceFilePath: string;
};

export type ApplyPatchAction = {
  kind: "apply-patch";
  patchContentPath: PatchContentPath;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "migrate-db";
} & ActionCommon;

export type Action = ApplyPatchAction | MigrateDbAction;

export function createApplyPatchAction(
  commonActionData: ActionCommon,
): ApplyPatchAction {
  const markdownFileWithoutExt = path.basename(
    commonActionData.markdownSourceFilePath,
    path.extname(commonActionData.markdownSourceFilePath),
  );
  const patchContentPath = path.resolve(
    "../docs/tutorial",
    "patches",
    `${markdownFileWithoutExt}__${commonActionData.stepName}.patch`,
  ) as PatchContentPath;

  return {
    ...commonActionData,
    kind: "apply-patch",
    patchContentPath,
  };
}
