import { describe, expect, it } from "vitest";

import {
  filterValidTutorialFileNames,
  sortFileNamesByNumberedPrefix,
} from "../../src/extract-actions/fileOperations";

describe("filterValidTutorialFileNames", () => {
  function testFilteringTutorialFileNames(
    testName: string,
    inputFiles: string[],
    expectedFiles: string[],
  ) {
    it(testName, () => {
      expect(filterValidTutorialFileNames(inputFiles)).toEqual(expectedFiles);
    });
  }

  testFilteringTutorialFileNames(
    "should return empty array when no markdown files are present",
    ["file.txt", "image.png", "document.pdf"],
    [],
  );

  testFilteringTutorialFileNames(
    "should filter files ending with .md or .mdx",
    [
      "01-intro.md",
      "02-setup.mdx",
      "03-advanced.md",
      "README.txt",
      "package.json",
    ],
    ["01-intro.md", "02-setup.mdx", "03-advanced.md"],
  );

  testFilteringTutorialFileNames(
    "should handle case-insensitive file extensions",
    ["README.MD", "guide.MDX", "tutorial.Md", "docs.mDx", "file.txt"],
    ["README.MD", "guide.MDX", "tutorial.Md", "docs.mDx"],
  );

  testFilteringTutorialFileNames(
    "should not match extensions in the middle of filename",
    ["file.md.txt", "file.mdx.backup", "README.md"],
    ["README.md"],
  );
});

describe("sortFileNamesByNumberedPrefix", () => {
  it("should sort files by numeric prefix", () => {
    const files = ["03-advanced.md", "01-intro.md", "02-setup.md"];

    const result = sortFileNamesByNumberedPrefix(files);

    expect(result).toEqual(["01-intro.md", "02-setup.md", "03-advanced.md"]);
  });
});
