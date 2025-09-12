import type { Branded } from "../brandedTypes";

// If modify or add new action kinds, make sure to also update the type in `web/docs/tutorial/TutorialAction.tsx`.
export type Action = InitAppAction | ApplyPatchAction | MigrateDbAction;

export interface InitAppAction extends ActionCommon {
  kind: "INIT_APP";
  waspStarterTemplateName: string;
}

export interface ApplyPatchAction extends ActionCommon {
  kind: "APPLY_PATCH";
  displayName: string;
  patchFilePath: PatchFilePath;
}

export interface MigrateDbAction extends ActionCommon {
  kind: "MIGRATE_DB";
}

export interface ActionCommon {
  id: ActionId;
  tutorialFilePath: MarkdownFilePath;
}

export type ActionId = Branded<string, "ActionId">;
export type PatchFilePath = Branded<string, "PatchFilePath">;
export type MarkdownFilePath = Branded<string, "MarkdownFilePath">;
