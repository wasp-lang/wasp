import { describe, expect, test } from "vitest";
import { getExtImportPathForRefImport } from "../../../src/spec-pipeline/planImportLowering/extImportPath.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("getExtImportPathForRefImport", () => {
  const projectRootDir = "/project";

  test("maps a top-level spec ref import targeting src", () => {
    expect(
      getExtImportPathForRefImport({
        refImportPath: "./src/MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("maps a nested spec ref import from its importing file", () => {
    expect(
      getExtImportPathForRefImport({
        refImportPath: "./MainPage",
        importingFilePath: `${projectRootDir}/src/features/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/features/MainPage");
  });

  test("maps a nested ref import that climbs to the src root", () => {
    expect(
      getExtImportPathForRefImport({
        refImportPath: "../../MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("normalizes ref import path segments", () => {
    expect(
      getExtImportPathForRefImport({
        refImportPath: "./src/features/../MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("rejects ref imports that resolve outside src", () => {
    expect(() =>
      getExtImportPathForRefImport({
        refImportPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(SpecUserError);
    expect(() =>
      getExtImportPathForRefImport({
        refImportPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(/must resolve to a file inside the app src\/ directory/);
  });

  test("rejects ref imports that resolve to src itself", () => {
    expect(() =>
      getExtImportPathForRefImport({
        refImportPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(SpecUserError);
    expect(() =>
      getExtImportPathForRefImport({
        refImportPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrowError(/must resolve to a file inside the app src\/ directory/);
  });

  test("allows ref imports that leave src and resolve back inside", () => {
    expect(
      getExtImportPathForRefImport({
        refImportPath: "../../../src/MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });
});
