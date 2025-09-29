import type { MdxJsxAttribute, MdxJsxFlowElement } from "mdast-util-mdx-jsx";
import { describe, expect, it } from "vitest";

import {
  filterTutorialFileNames,
  getAttributeValue,
  sortTutorialFileNames,
} from "../../src/extract-actions/index";

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

describe("filterTutorialFileNames", () => {
  it("should filter files ending with .md", () => {
    const files = ["01-intro.md", "02-setup.md", "README.txt", "package.json"];

    const result = filterTutorialFileNames(files);

    expect(result).toEqual(["01-intro.md", "02-setup.md"]);
  });
});

describe("sortTutorialFileNames", () => {
  it("should sort files by numeric prefix", () => {
    const files = ["03-advanced.md", "01-intro.md", "02-setup.md"];

    const result = sortTutorialFileNames(files);

    expect(result).toEqual(["01-intro.md", "02-setup.md", "03-advanced.md"]);
  });
});
