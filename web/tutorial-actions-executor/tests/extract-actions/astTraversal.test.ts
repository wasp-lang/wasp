import type { Root } from "mdast";
import type { MdxJsxAttribute, MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { describe, expect, it } from "vitest";

import type { ActionId } from "../../src/actions/actions.ts";
import {
  createTutorialActionNode,
  extractTutorialActionNodes,
  getAttributeValue,
  TUTORIAL_ACTION_NODE_NAME,
  type TutorialActionNode,
} from "../../src/extract-actions/astTraversal.ts";

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

describe("createTutorialActionNode", () => {
  it("should create node with all attributes", () => {
    const result = createTutorialActionNode({
      id: "test-id",
      action: "APPLY_PATCH",
      starterTemplateName: "basic",
      packageSpecifiers: null,
    });
    expect(result).toEqual({
      id: "test-id",
      action: "APPLY_PATCH",
      starterTemplateName: "basic",
      packageSpecifiers: null,
    });
  });

  it("should create node without starterTemplateName", () => {
    const result = createTutorialActionNode({
      id: "test-id",
      action: "MIGRATE_DB",
      starterTemplateName: null,
      packageSpecifiers: null,
    });
    expect(result).toEqual({
      id: "test-id",
      action: "MIGRATE_DB",
      starterTemplateName: null,
      packageSpecifiers: null,
    });
  });

  it("should throw error when id is null", () => {
    expect(() =>
      createTutorialActionNode({
        id: null,
        action: "APPLY_PATCH",
        starterTemplateName: null,
        packageSpecifiers: null,
      }),
    ).toThrow("TutorialAction component requires the 'id' attribute");
  });

  it("should throw error when id is empty string", () => {
    expect(() =>
      createTutorialActionNode({
        id: "",
        action: "APPLY_PATCH",
        starterTemplateName: null,
        packageSpecifiers: null,
      }),
    ).toThrow("TutorialAction component requires the 'id' attribute");
  });

  it("should throw error when action is null", () => {
    expect(() =>
      createTutorialActionNode({
        id: "test-id",
        action: null,
        starterTemplateName: null,
        packageSpecifiers: null,
      }),
    ).toThrow("TutorialAction component requires the 'action' attribute");
  });

  it("should throw error when action is empty string", () => {
    expect(() =>
      createTutorialActionNode({
        id: "test-id",
        action: "",
        starterTemplateName: null,
        packageSpecifiers: null,
      }),
    ).toThrow("TutorialAction component requires the 'action' attribute");
  });

  it("should split packageSpecifiers by spaces", () => {
    const result = createTutorialActionNode({
      id: "test-id",
      action: "INSTALL_PACKAGES",
      starterTemplateName: null,
      packageSpecifiers: " lucia@3  arctic@1 ",
    });
    expect(result.packageSpecifiers).toEqual(["lucia@3", "arctic@1"]);
  });

  it("should have no packageSpecifiers when the attribute is blank", () => {
    const result = createTutorialActionNode({
      id: "test-id",
      action: "INSTALL_PACKAGES",
      starterTemplateName: null,
      packageSpecifiers: " ",
    });
    expect(result.packageSpecifiers).toEqual([]);
  });
});

describe("extractTutorialActionNodes", () => {
  function createMockTutorialActionNode(
    id: string,
    action: string,
    starterTemplateName?: string,
  ): MdxJsxFlowElement {
    const attributes: MdxJsxAttribute[] = [
      { type: "mdxJsxAttribute", name: "id", value: id },
      { type: "mdxJsxAttribute", name: "action", value: action },
    ];
    if (starterTemplateName) {
      attributes.push({
        type: "mdxJsxAttribute",
        name: "starterTemplateName",
        value: starterTemplateName,
      });
    }
    return {
      type: "mdxJsxFlowElement",
      name: TUTORIAL_ACTION_NODE_NAME,
      attributes,
      children: [],
    } as MdxJsxFlowElement;
  }

  function createMockNonTutorialActionNode(name: string): MdxJsxFlowElement {
    return {
      type: "mdxJsxFlowElement",
      name,
      attributes: [],
      children: [],
    } as MdxJsxFlowElement;
  }

  function createMockAst(children: unknown[]): Root {
    return {
      type: "root",
      children,
    } as Root;
  }

  function testExtractingTutorialActionNodes(
    testName: string,
    ast: Root,
    expectedResult: TutorialActionNode[],
  ) {
    it(testName, () => {
      const result = extractTutorialActionNodes(ast);
      expect(result).toEqual(expectedResult);
    });
  }

  testExtractingTutorialActionNodes(
    "should return empty array for empty AST",
    createMockAst([]),
    [],
  );

  testExtractingTutorialActionNodes(
    "should extract single TutorialAction node",
    createMockAst([createMockTutorialActionNode("test-id", "APPLY_PATCH")]),
    [
      {
        id: "test-id" as ActionId,
        action: "APPLY_PATCH",
        starterTemplateName: null,
        packageSpecifiers: null,
      },
    ],
  );

  testExtractingTutorialActionNodes(
    "should extract multiple TutorialAction nodes",
    createMockAst([
      createMockTutorialActionNode("init", "INIT_APP", "basic"),
      createMockTutorialActionNode("patch", "APPLY_PATCH"),
      createMockTutorialActionNode("migrate", "MIGRATE_DB"),
    ]),
    [
      {
        id: "init" as ActionId,
        action: "INIT_APP",
        starterTemplateName: "basic",
        packageSpecifiers: null,
      },
      {
        id: "patch" as ActionId,
        action: "APPLY_PATCH",
        starterTemplateName: null,
        packageSpecifiers: null,
      },
      {
        id: "migrate" as ActionId,
        action: "MIGRATE_DB",
        starterTemplateName: null,
        packageSpecifiers: null,
      },
    ],
  );

  testExtractingTutorialActionNodes(
    "should ignore non-TutorialAction MDX components",
    createMockAst([
      createMockNonTutorialActionNode("SomeComponent"),
      createMockTutorialActionNode("valid", "MIGRATE_DB"),
      createMockNonTutorialActionNode("AnotherComponent"),
    ]),
    [
      {
        id: "valid" as ActionId,
        action: "MIGRATE_DB",
        starterTemplateName: null,
        packageSpecifiers: null,
      },
    ],
  );

  testExtractingTutorialActionNodes(
    "should extract TutorialAction with optional starterTemplateName",
    createMockAst([
      createMockTutorialActionNode("with-template", "INIT_APP", "advanced"),
      createMockTutorialActionNode("without-template", "APPLY_PATCH"),
    ]),
    [
      {
        id: "with-template" as ActionId,
        action: "INIT_APP",
        starterTemplateName: "advanced",
        packageSpecifiers: null,
      },
      {
        id: "without-template" as ActionId,
        action: "APPLY_PATCH",
        starterTemplateName: null,
        packageSpecifiers: null,
      },
    ],
  );
});
