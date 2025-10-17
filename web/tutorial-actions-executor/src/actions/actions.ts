import type { Branded } from "../brandedTypes";

// If you modify or add new action kinds, make sure to also update the type in `web/docs/tutorial/TutorialAction.tsx`.
export type Action = InitAppAction | ApplyPatchAction | MigrateDbAction;

export interface InitAppAction extends BaseAction {
  kind: "INIT_APP";
  waspStarterTemplateName: string;
}

export interface ApplyPatchAction extends BaseAction {
  kind: "APPLY_PATCH";
  displayName: string;
  patchFilePath: PatchFilePath;
}

export interface MigrateDbAction extends BaseAction {
  kind: "MIGRATE_DB";
}

export interface BaseAction {
  id: ActionId;
  sourceTutorialFilePath: MdxFilePath;
}

export type ActionId = Branded<string, "ActionId">;
export type PatchFilePath = Branded<string, "PatchFilePath">;
export type MdxFilePath = Branded<string, "MdxFilePath">;
