import type { Branded } from "../brandedTypes";

// If modify or add new action kinds, make sure to also update the type in `web/docs/tutorial/TutorialAction.tsx`.
export type Action = InitAppAction | ApplyPatchAction | MigrateDbAction;

export type InitAppAction = {
  kind: "INIT_APP";
  waspStarterTemplateName: string;
} & ActionCommon;

export type ApplyPatchAction = {
  kind: "APPLY_PATCH";
  displayName: string;
  patchFilePath: PatchFilePath;
} & ActionCommon;

export type MigrateDbAction = {
  kind: "MIGRATE_DB";
} & ActionCommon;

export type ActionCommon = {
  id: ActionId;
  tutorialFilePath: MarkdownFilePath;
};

export type ActionId = Branded<string, "ActionId">;
export type PatchFilePath = Branded<string, "PatchFilePath">;
export type MarkdownFilePath = Branded<string, "MarkdownFilePath">;
