import { describe, expect, test } from "vitest";
import { mapImportPath } from "../../../src/spec-pipeline/lowerImportsPlugin/mapImportPath.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("mapImportPath", () => {
  const projectRootDir = "/project";

  test("maps a top-level spec ref import targeting src", () => {
    expect(
      mapImportPath({
        refImportPath: "./src/MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("maps a nested spec ref import from its importing file", () => {
    expect(
      mapImportPath({
        refImportPath: "./MainPage",
        importingFilePath: `${projectRootDir}/src/features/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/features/MainPage");
  });

  test("maps a nested ref import that climbs to the src root", () => {
    expect(
      mapImportPath({
        refImportPath: "../../MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("normalizes ref import path segments", () => {
    expect(
      mapImportPath({
        refImportPath: "./src/features/../MainPage",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });

  test("rejects ref imports that resolve outside src", () => {
    expect(() =>
      mapImportPath({
        refImportPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrow(SpecUserError);
    expect(() =>
      mapImportPath({
        refImportPath: "./helpers/helper",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrow(/must resolve to a file inside the app src\/ directory/);
  });

  test("rejects ref imports that resolve to src itself", () => {
    expect(() =>
      mapImportPath({
        refImportPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrow(SpecUserError);
    expect(() =>
      mapImportPath({
        refImportPath: "./src",
        importingFilePath: `${projectRootDir}/main.wasp.ts`,
        projectRootDir,
      }),
    ).toThrow(/must resolve to a file inside the app src\/ directory/);
  });

  test("allows ref imports that leave src and resolve back inside", () => {
    expect(
      mapImportPath({
        refImportPath: "../../../src/MainPage",
        importingFilePath: `${projectRootDir}/src/features/home/home.wasp.ts`,
        projectRootDir,
      }),
    ).toBe("@src/MainPage");
  });
});
