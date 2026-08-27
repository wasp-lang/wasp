import { describe, expect, test } from "vitest";
import { normalizeRefObjectPath } from "../../src/spec/refObjectPath.js";
import { WaspSpecUserError } from "../../src/spec/waspSpecUserError.js";

describe("normalizeRefObjectPath", () => {
  test("maps a top-level spec ref targeting src", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "./src/MainPage",
        specFilePath: "main.wasp.ts",
        originKind: "project",
      }),
    ).toBe("MainPage");
  });

  test("maps a nested spec ref from its logical path", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "./MainPage",
        specFilePath: "src/features/home.wasp.ts",
        originKind: "project",
      }),
    ).toBe("features/MainPage");
  });

  test("maps a nested package ref and strips its extension", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "../../MainPage.tsx",
        specFilePath: "src/features/home/home.wasp.ts",
        originKind: "package",
      }),
    ).toBe("MainPage");
  });

  test("normalizes ref path segments", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "./src/features/../MainPage",
        specFilePath: "main.wasp.ts",
        originKind: "project",
      }),
    ).toBe("MainPage");
  });

  test("rejects refs that resolve outside src", () => {
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./helpers/helper",
        specFilePath: "main.wasp.ts",
        originKind: "project",
      }),
    ).toThrow(WaspSpecUserError);
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./helpers/helper",
        specFilePath: "main.wasp.ts",
        originKind: "project",
      }),
    ).toThrow(/must resolve to a file inside the app src\/ directory/);
  });

  test("rejects refs that resolve to src itself", () => {
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./src",
        specFilePath: "module.wasp.ts",
        originKind: "package",
      }),
    ).toThrow(/must resolve to a file inside the package src\/ directory/);
  });

  test("allows refs that leave src and resolve back inside", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "../../../src/MainPage",
        specFilePath: "src/features/home/home.wasp.ts",
        originKind: "project",
      }),
    ).toBe("MainPage");
  });

  test.each(["", "/main.wasp.ts", "C:\\main.wasp.ts", "../main.wasp.ts"])(
    "rejects invalid logical spec path %j",
    (specFilePath) => {
      expect(() =>
        normalizeRefObjectPath({
          importPath: "./src/MainPage",
          specFilePath,
          originKind: "project",
        }),
      ).toThrow(/must be relative to its project or package root/);
    },
  );
});
