import type { Action, BaseAction, MdxFilePath } from "../actions/actions.ts";
import {
  createApplyPatchAction,
  createInitAppAction,
  createMigrateDbAction,
} from "../actions/index.ts";
import { assertUnreachable } from "../assert.ts";
import type { PatchesDirPath } from "../tutorialApp.ts";
import type { TutorialActionNode } from "./astTraversal.ts";

export function mapTutorialActionNodeToAction(
  node: TutorialActionNode,
  sourceTutorialFilePath: MdxFilePath,
  patchesPath: PatchesDirPath,
): Action {
  const commonData: BaseAction = {
    id: node.id,
    sourceTutorialFilePath,
  };

  switch (node.action) {
    case "INIT_APP":
      if (node.starterTemplateName === null) {
        throw new Error(
          `TutorialAction with action 'INIT_APP' requires the 'starterTemplateName' attribute.`,
        );
      }
      return createInitAppAction(commonData, node.starterTemplateName);
    case "APPLY_PATCH":
      return createApplyPatchAction(commonData, patchesPath);
    case "MIGRATE_DB":
      return createMigrateDbAction(commonData);
    default:
      assertUnreachable(
        node.action,
        `Unknown action '${node.action}' in TutorialAction component.`,
      );
  }
}
