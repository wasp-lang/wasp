import { describe, expect, test } from "vitest";
import { getRefObjectPathForRefImport } from "../../../src/spec-pipeline/planImportLowering/refObjectPath.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("getRefObjectPathForRefImport", () => {
  const projectRootDir = "/project";

  test("maps a top-level spec ref import targeting src", () => {
    expect(
      getRefObjectPathForRefImport({
        refImportPath: "./src/MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("maps a nested spec ref import from its importing file", () => {
    expect(
      getRefObjectPathForRefImport({
        refImportPath: "./MainPage",
        importingFilePath: `${projectRootDir}/src/features/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/features/MainPage");
  });

  test("maps a nested ref import that climbs to the src root", () => {
    expect(
      getRefObjectPathForRefImport({
        refImportPath: "../../MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("normalizes ref import path segments", () => {
    expect(
      getRefObjectPathForRefImport({
        refImportPath: "./src/features/../MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("rejects ref imports that resolve outside src", () => {
    expect(() =>
      getRefObjectPathForRefImport({
        refImportPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(SpecUserError);
    expect(() =>
      getRefObjectPathForRefImport({
        refImportPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(/must resolve to a file inside the app src\/ directory/);
  });

  test("rejects ref imports that resolve to src itself", () => {
    expect(() =>
      getRefObjectPathForRefImport({
        refImportPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(SpecUserError);
    expect(() =>
      getRefObjectPathForRefImport({
        refImportPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(/must resolve to a file inside the app src\/ directory/);
  });

  test("allows ref imports that leave src and resolve back inside", () => {
    expect(
      getRefObjectPathForRefImport({
        refImportPath: "../../../src/MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });
});
