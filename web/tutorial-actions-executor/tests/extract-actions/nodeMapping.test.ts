import { describe, expect, it } from "vitest";

import type {
  Action,
  ActionId,
  MdxFilePath,
  PatchFilePath,
} from "../../src/actions/actions.ts";
import type { TutorialActionNode } from "../../src/extract-actions/astTraversal.ts";
import { mapTutorialActionNodeToAction } from "../../src/extract-actions/nodeMapping.ts";
import type { PatchesDirPath } from "../../src/tutorialApp.ts";

describe("mapTutorialActionNodeToAction", () => {
  const testContext = {
    filePath: "/test/tutorial/01-intro.md" as MdxFilePath,
    patchesPath: "/test/tutorial/patches" as PatchesDirPath,
  };

  function createNode({
    id,
    action,
    starterTemplateName = null,
    packageSpecifiers = null,
  }: {
    id: string;
    action: Action["kind"];
    starterTemplateName?: string | null;
    packageSpecifiers?: string[] | null;
  }): TutorialActionNode {
    return {
      id: id as ActionId,
      action,
      starterTemplateName,
      packageSpecifiers,
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
    createNode({
      id: "init-project",
      action: "INIT_APP",
      starterTemplateName: "basic",
    }),
    {
      id: "init-project" as ActionId,
      sourceTutorialFilePath: testContext.filePath,
      kind: "INIT_APP",
      waspStarterTemplateName: "basic",
    },
  );

  testMappingNodeThrows(
    "should throw error when INIT_APP action is missing starterTemplateName",
    createNode({ id: "init-project", action: "INIT_APP" }),
    "TutorialAction with action 'INIT_APP' requires the 'starterTemplateName' attribute",
  );

  testMappingNode(
    "should map APPLY_PATCH action",
    createNode({ id: "add-feature", action: "APPLY_PATCH" }),
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
    createNode({ id: "migrate-schema", action: "MIGRATE_DB" }),
    {
      id: "migrate-schema" as ActionId,
      sourceTutorialFilePath: testContext.filePath,
      kind: "MIGRATE_DB",
    },
  );

  testMappingNode(
    "should map INSTALL_PACKAGES action with its package specifiers",
    createNode({
      id: "install-auth",
      action: "INSTALL_PACKAGES",
      packageSpecifiers: ["lucia@3", "arctic@1"],
    }),
    {
      id: "install-auth" as ActionId,
      sourceTutorialFilePath: testContext.filePath,
      kind: "INSTALL_PACKAGES",
      packageSpecifiers: ["lucia@3", "arctic@1"],
    },
  );

  testMappingNodeThrows(
    "should throw error when INSTALL_PACKAGES action is missing package specifiers",
    createNode({ id: "install-auth", action: "INSTALL_PACKAGES" }),
    "TutorialAction with action 'INSTALL_PACKAGES' requires the 'packageSpecifiers' attribute",
  );

  testMappingNodeThrows(
    "should throw error when INSTALL_PACKAGES action has no package specifiers",
    createNode({
      id: "install-auth",
      action: "INSTALL_PACKAGES",
      packageSpecifiers: [],
    }),
    "TutorialAction with action 'INSTALL_PACKAGES' requires the 'packageSpecifiers' attribute",
  );

  testMappingNodeThrows(
    "should throw error for unknown action type",
    createNode({
      id: "test-action",
      action: "UNKNOWN_ACTION" as Action["kind"],
    }),
    "Unknown action 'UNKNOWN_ACTION' in TutorialAction component",
  );
});
