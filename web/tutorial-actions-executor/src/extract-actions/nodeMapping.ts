import type { Action, BaseAction, MdxFilePath } from "../actions/actions.js";
import {
  createApplyPatchAction,
  createInitAppAction,
  createMigrateDbAction,
} from "../actions/index.js";
import { assertUnreachable } from "../assert.js";
import type { PatchesDirPath } from "../tutorialApp.js";
import type { TutorialActionNode } from "./astTraversal.js";

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
