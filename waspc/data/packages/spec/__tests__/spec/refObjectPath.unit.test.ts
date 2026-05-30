import { describe, expect, test } from "vitest";
import { normalizeRefObjectPath } from "../../src/spec/refObjectPath.js";
import { SpecUserError } from "../../src/spec/specUserError.js";

describe("normalizeRefObjectPath", () => {
  const projectRootDir = "/project";

  test("maps a top-level spec ref import targeting src", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "./src/MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("maps a nested spec ref import from its importing file", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "./MainPage",
        importingFilePath: `${projectRootDir}/src/features/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/features/MainPage");
  });

  test("maps a nested ref import that climbs to the src root", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "../../MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("normalizes ref import path segments", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "./src/features/../MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("rejects ref imports that resolve outside src", () => {
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(SpecUserError);
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(/must resolve to a file inside the app src\/ directory/);
  });

  test("rejects ref imports that resolve to src itself", () => {
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(SpecUserError);
    expect(() =>
      normalizeRefObjectPath({
        importPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(/must resolve to a file inside the app src\/ directory/);
  });

  test("allows ref imports that leave src and resolve back inside", () => {
    expect(
      normalizeRefObjectPath({
        importPath: "../../../src/MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });
});
