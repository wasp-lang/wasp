import type { MdxJsxAttribute, MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { describe, expect, it } from "vitest";

import type { MdxFilePath } from "../../src/actions/actions";
import {
  getActionsFromMdxContent,
  getAttributeValue,
  getMarkdownFileNames,
  sortFileNamesByNumberedPrefix,
} from "../../src/extract-actions/index";
import type {
  AppDirPath,
  AppName,
  AppParentDirPath,
  PatchesDirPath,
  TutorialDirPath,
} from "../../src/tutorialApp";
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
} from "./exampleMdxFiles";

describe("getAttributeValue", () => {
  it("should return correct value when attributes exist", () => {
    const mockNode = {
      attributes: [
        {
          type: "mdxJsxAttribute",
          name: "action",
          value: "APPLY_PATCH",
        },
        {
          type: "mdxJsxAttribute",
          name: "id",
          value: "test-action",
        },
      ],
    } as MdxJsxFlowElement;

    expect(getAttributeValue(mockNode, "id")).toBe("test-action");
    expect(getAttributeValue(mockNode, "action")).toBe("APPLY_PATCH");
  });

  it("should return null when attribute does not exist", () => {
    const mockNode = {
      attributes: [] as MdxJsxAttribute[],
    } as MdxJsxFlowElement;

    expect(getAttributeValue(mockNode, "id")).toBe(null);
  });
});

describe("getMarkdownFileNames", () => {
  it("should filter files ending with .md", () => {
    const files = ["01-intro.md", "02-setup.md", "README.txt", "package.json"];

    const result = getMarkdownFileNames(files);

    expect(result).toEqual(["01-intro.md", "02-setup.md"]);
  });
});

describe("sortFileNamesByNumberedPrefix", () => {
  it("should sort files by numeric prefix", () => {
    const files = ["03-advanced.md", "01-intro.md", "02-setup.md"];

    const result = sortFileNamesByNumberedPrefix(files);

    expect(result).toEqual(["01-intro.md", "02-setup.md", "03-advanced.md"]);
  });
});

describe("getActionsFromMdxContent", () => {
  const context = {
    tutorialApp: {
      name: "TestApp" as AppName,
      parentDirPath: "/test/parent" as AppParentDirPath,
      dirPath: "/test/parent/TestApp" as AppDirPath,
      docsTutorialDirPath: "/test/tutorial" as TutorialDirPath,
      docsTutorialPatchesPath: "/test/tutorial/patches" as PatchesDirPath,
    },
    filePath: "/test/tutorial/01-intro.md" as MdxFilePath,
  };

  function itShouldExtractActions(
    testName: string,
    mdxContent: string,
    expectedActions: unknown[],
  ) {
    it(`should extract actions: ${testName}`, async () => {
      const actions = await getActionsFromMdxContent(
        context.filePath,
        Buffer.from(mdxContent),
        context.tutorialApp,
      );
      expect(actions).toEqual(expectedActions);
    });
  }

  function itShouldFailWhenExtractingActions(
    testName: string,
    mdxContent: string,
    errorMessage: string,
  ) {
    it(`should fail extracting actions: ${testName}`, async () => {
      await expect(
        getActionsFromMdxContent(
          context.filePath,
          Buffer.from(mdxContent),
          context.tutorialApp,
        ),
      ).rejects.toThrow(errorMessage);
    });
  }

  itShouldExtractActions("INIT_APP action", mdxWithInitAppAction, [
    {
      id: "init-project",
      sourceTutorialFilePath: context.filePath,
      kind: "INIT_APP",
      waspStarterTemplateName: "basic",
    },
  ]);

  itShouldExtractActions("APPLY_PATCH action", mdxWithApplyPatchAction, [
    {
      id: "add-feature",
      sourceTutorialFilePath: context.filePath,
      kind: "APPLY_PATCH",
      displayName: "01-intro.md / add-feature",
      patchFilePath: "/test/tutorial/patches/01-intro__add-feature.patch",
    },
  ]);

  itShouldExtractActions("MIGRATE_DB action", mdxWithMigrateDbAction, [
    {
      id: "migrate-schema",
      sourceTutorialFilePath: context.filePath,
      kind: "MIGRATE_DB",
    },
  ]);

  itShouldExtractActions("multiple actions", mdxWithMultipleActions, [
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

  itShouldExtractActions("no actions present", mdxWithNoActions, []);

  itShouldExtractActions(
    "should ignore non-TutorialAction MDX components",
    mdxWithNonTutorialActionComponents,
    [
      {
        id: "valid-action",
        sourceTutorialFilePath: context.filePath,
        kind: "MIGRATE_DB",
      },
    ],
  );

  itShouldFailWhenExtractingActions(
    "when action attribute is missing",
    mdxWithMissingActionAttribute,
    "TutorialAction component requires the 'action' attribute",
  );

  itShouldFailWhenExtractingActions(
    "when id attribute is missing",
    mdxWithMissingIdAttribute,
    "TutorialAction component requires the 'id' attribute",
  );

  itShouldFailWhenExtractingActions(
    "when INIT_APP action is missing starterTemplateName",
    mdxWithMissingStarterTemplateName,
    "TutorialAction with action 'INIT_APP' requires the 'starterTemplateName' attribute",
  );

  itShouldFailWhenExtractingActions(
    "for unknown action type",
    mdxWithUnknownActionType,
    "Unknown action 'UNKNOWN_ACTION' in TutorialAction component",
  );
});
