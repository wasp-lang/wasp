import { describe, expect, it } from "vitest";

import type {
  Action,
  ActionId,
  MdxFilePath,
  PatchFilePath,
} from "../../src/actions/actions";
import type { TutorialActionNode } from "../../src/extract-actions/astTraversal";
import { mapTutorialActionNodeToAction } from "../../src/extract-actions/nodeMapping";
import type { PatchesDirPath } from "../../src/tutorialApp";

describe("mapTutorialActionNodeToAction", () => {
  const testContext = {
    filePath: "/test/tutorial/01-intro.md" as MdxFilePath,
    patchesPath: "/test/tutorial/patches" as PatchesDirPath,
  };

  function createNode(
    id: string,
    action: Action["kind"],
    starterTemplateName: string | null = null,
  ): TutorialActionNode {
    return {
      id: id as ActionId,
      action,
      starterTemplateName,
    };
  }

  function testMappingNode(
    testName: string,
    node: TutorialActionNode,
    expectedResult: Action,
  ) {
    it(testName, () => {
      const result = mapTutorialActionNodeToAction(
        node,
        testContext.filePath,
        testContext.patchesPath,
      );
      expect(result).toEqual(expectedResult);
    });
  }

  function testMappingNodeThrows(
    testName: string,
    node: TutorialActionNode,
    errorMessage: string,
  ) {
    it(testName, () => {
      expect(() =>
        mapTutorialActionNodeToAction(
          node,
          testContext.filePath,
          testContext.patchesPath,
        ),
      ).toThrow(errorMessage);
    });
  }

  testMappingNode(
    "should map INIT_APP action with starterTemplateName",
    createNode("init-project", "INIT_APP", "basic"),
    {
      id: "init-project" as ActionId,
      sourceTutorialFilePath: testContext.filePath,
      kind: "INIT_APP",
      waspStarterTemplateName: "basic",
    },
  );

  testMappingNodeThrows(
    "should throw error when INIT_APP action is missing starterTemplateName",
    createNode("init-project", "INIT_APP", null),
    "TutorialAction with action 'INIT_APP' requires the 'starterTemplateName' attribute",
  );

  testMappingNode(
    "should map APPLY_PATCH action",
    createNode("add-feature", "APPLY_PATCH"),
    {
      id: "add-feature" as ActionId,
      sourceTutorialFilePath: testContext.filePath,
      kind: "APPLY_PATCH",
      displayName: "01-intro.md / add-feature",
      patchFilePath:
        "/test/tutorial/patches/01-intro__add-feature.patch" as PatchFilePath,
    },
  );

  testMappingNode(
    "should map MIGRATE_DB action",
    createNode("migrate-schema", "MIGRATE_DB"),
    {
      id: "migrate-schema" as ActionId,
      sourceTutorialFilePath: testContext.filePath,
      kind: "MIGRATE_DB",
    },
  );

  testMappingNodeThrows(
    "should throw error for unknown action type",
    createNode("test-action", "UNKNOWN_ACTION" as Action["kind"]),
    "Unknown action 'UNKNOWN_ACTION' in TutorialAction component",
  );
});
