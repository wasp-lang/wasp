import type { Branded } from "../brandedTypes";

// If modify or add new action kinds, make sure to also update the type in `docs/tutorial/TutorialAction.tsx`.
export type Action = ApplyPatchAction | MigrateDbAction;

export type ApplyPatchAction = {
  kind: "apply-patch";
  displayName: string;
  patchFilePath: PatchFilePath;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "migrate-db";
} & ActionCommon;

export type ActionCommon = {
  id: ActionId;
  tutorialFilePath: MarkdownFilePath;
};

export type ActionId = Branded<string, "ActionId">;
export type PatchFilePath = Branded<string, "PatchFilePath">;
export type MarkdownFilePath = Branded<string, "MarkdownFilePath">;
