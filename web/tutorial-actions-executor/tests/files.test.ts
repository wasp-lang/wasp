import { describe, expect, it } from "vitest";

import { getFileNameWithoutExtension } from "../src/files";

describe("getFileNameWithoutExtension", () => {
  testGettingFileNameWithoutExtension("test.txt", "test");
  testGettingFileNameWithoutExtension("test.config.js", "test.config");
  testGettingFileNameWithoutExtension("test", "test");
  testGettingFileNameWithoutExtension("/path/to/test.txt", "test");
  testGettingFileNameWithoutExtension("./src/test.ts", "test");
  testGettingFileNameWithoutExtension("", "");
});

function testGettingFileNameWithoutExtension(
  filePath: string,
  expectedFileName: string,
) {
  it(`should return "${expectedFileName}" for "${filePath}"`, () => {
    expect(getFileNameWithoutExtension(filePath)).toBe(expectedFileName);
  });
}
