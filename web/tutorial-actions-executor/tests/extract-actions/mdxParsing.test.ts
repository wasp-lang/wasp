import { describe, expect, it } from "vitest";

import type { MdxFilePath } from "../../src/actions/actions";
import { getActionsFromMdxContent } from "../../src/extract-actions/mdxParsing";
import type { PatchesDirPath } from "../../src/tutorialApp";
import {
  mdxWithApplyPatchAction,
  mdxWithInitAppAction,
  mdxWithMigrateDbAction,
  mdxWithMissingActionAttribute,
  mdxWithMissingIdAttribute,
  mdxWithMissingStarterTemplateName,
  mdxWithMultipleActions,
  mdxWithNoActions,
  mdxWithNonTutorialActionComponents,
  mdxWithUnknownActionType,
} from "./mdxFileFixtures";

describe("getActionsFromMdxContent", () => {
  const context = {
    filePath: "/test/tutorial/01-intro.md" as MdxFilePath,
    patchesPath: "/test/tutorial/patches" as PatchesDirPath,
  };

  function testExtractingActions(
    testName: string,
    mdxContent: string,
    expectedActions: unknown[],
  ) {
    it(`should extract actions: ${testName}`, async () => {
      const actions = await getActionsFromMdxContent(
        Buffer.from(mdxContent),
        context.filePath,
        context.patchesPath,
      );
      expect(actions).toEqual(expectedActions);
    });
  }

  function testExtractingActionsThrows(
    testName: string,
    mdxContent: string,
    errorMessage: string,
  ) {
    it(`should fail extracting actions: ${testName}`, async () => {
      await expect(
        getActionsFromMdxContent(
          Buffer.from(mdxContent),
          context.filePath,
          context.patchesPath,
        ),
      ).rejects.toThrow(errorMessage);
    });
  }

  testExtractingActions("INIT_APP action", mdxWithInitAppAction, [
    {
      id: "init-project",
      sourceTutorialFilePath: context.filePath,
      kind: "INIT_APP",
      waspStarterTemplateName: "basic",
    },
  ]);

  testExtractingActions("APPLY_PATCH action", mdxWithApplyPatchAction, [
    {
      id: "add-feature",
      sourceTutorialFilePath: context.filePath,
      kind: "APPLY_PATCH",
      displayName: "01-intro.md / add-feature",
      patchFilePath: "/test/tutorial/patches/01-intro__add-feature.patch",
    },
  ]);

  testExtractingActions("MIGRATE_DB action", mdxWithMigrateDbAction, [
    {
      id: "migrate-schema",
      sourceTutorialFilePath: context.filePath,
      kind: "MIGRATE_DB",
    },
  ]);

  testExtractingActions("multiple actions", mdxWithMultipleActions, [
    {
      id: "init-project",
      sourceTutorialFilePath: context.filePath,
      kind: "INIT_APP",
      waspStarterTemplateName: "basic",
    },
    {
      id: "add-feature",
      sourceTutorialFilePath: context.filePath,
      kind: "APPLY_PATCH",
      displayName: "01-intro.md / add-feature",
      patchFilePath: "/test/tutorial/patches/01-intro__add-feature.patch",
    },
    {
      id: "migrate-schema",
      sourceTutorialFilePath: context.filePath,
      kind: "MIGRATE_DB",
    },
  ]);

  testExtractingActions("no actions present", mdxWithNoActions, []);

  testExtractingActions(
    "ignore non-TutorialAction MDX components",
    mdxWithNonTutorialActionComponents,
    [
      {
        id: "valid-action",
        sourceTutorialFilePath: context.filePath,
        kind: "MIGRATE_DB",
      },
    ],
  );

  testExtractingActionsThrows(
    "when action attribute is missing",
    mdxWithMissingActionAttribute,
    "TutorialAction component requires the 'action' attribute",
  );

  testExtractingActionsThrows(
    "when id attribute is missing",
    mdxWithMissingIdAttribute,
    "TutorialAction component requires the 'id' attribute",
  );

  testExtractingActionsThrows(
    "when INIT_APP action is missing starterTemplateName",
    mdxWithMissingStarterTemplateName,
    "TutorialAction with action 'INIT_APP' requires the 'starterTemplateName' attribute",
  );

  testExtractingActionsThrows(
    "for unknown action type",
    mdxWithUnknownActionType,
    "Unknown action 'UNKNOWN_ACTION' in TutorialAction component",
  );
});
