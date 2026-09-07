import path from "node:path";
import { describe, expect, test } from "vitest";
import { getRootRelativeSpecFilePath } from "../../src/spec-pipeline/refOrigin.js";

describe("getRootRelativeSpecFilePath", () => {
  test("returns a normalized path inside the root", () => {
    expect(
      getRootRelativeSpecFilePath(
        path.join("project", "module"),
        path.join("project", "module", "features", "auth.wasp.ts"),
      ),
    ).toBe("features/auth.wasp.ts");
  });

  test("rejects a path outside the root", () => {
    expect(() =>
      getRootRelativeSpecFilePath(
        path.join("project", "module"),
        path.join("project", "other", "auth.wasp.ts"),
      ),
    ).toThrow(/must be inside/);
  });
});
