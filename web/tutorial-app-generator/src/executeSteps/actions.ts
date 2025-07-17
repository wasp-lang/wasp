import path from "path";

export type ActionCommon = {
  stepName: string;
  markdownSourceFilePath: string;
};

export type ApplyPatchAction = {
  kind: "apply-patch";
  patchContentPath: string;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "migrate-db";
} & ActionCommon;

export type Action = ApplyPatchAction | MigrateDbAction;

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
    kind: "apply-patch",
    patchContentPath,
  };
}
