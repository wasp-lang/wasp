import { pathToFileURL } from "node:url";
import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import {
  makeRefImport,
  mapRefImportToExtImport,
  refImport,
} from "../../src/spec/refImport.js";
import { SpecUserError } from "../../src/spec/specUserError.js";
import * as Fixtures from "./testFixtures.js";

describe("mapRefImportToExtImport", () => {
  const projectRootDir = "/project";

  test("should map minimal named import correctly", () => {
    testRefImportMapping(Fixtures.getRefImport("minimal", "named"), {
      kind: "named",
      name: "namedExport",
      path: "@src/external",
      alias: undefined,
    });
  });

  test("should map full named import correctly", () => {
    testRefImportMapping(Fixtures.getRefImport("full", "named"), {
      kind: "named",
      name: "namedExport",
      path: "@src/external",
      alias: "namedAlias",
    });
  });

  test("should map minimal default import correctly", () => {
    testRefImportMapping(Fixtures.getRefImport("minimal", "default"), {
      kind: "default",
      name: "defaultExport",
      path: "@src/external",
    });
  });

  test("should map full default import correctly", () => {
    testRefImportMapping(Fixtures.getRefImport("full", "default"), {
      kind: "default",
      name: "defaultExport",
      path: "@src/external",
    });
  });

  test.each([
    { extImport: () => null },
    { extImport: { parse: () => ({}) } },
    { extImport: { from: "@src/external" } },
  ])("returns an error for invalid runtime values", ({ extImport }) => {
    expect(() =>
      mapRefImportToExtImport(extImport, { projectRootDir }),
    ).toThrowError(SpecUserError);
    expect(() =>
      mapRefImportToExtImport(extImport, { projectRootDir }),
    ).toThrowError("Got an import in the Wasp file that we couldn't process");
  });

  test("normalizes source-aware relative paths", () => {
    expect(
      mapRefImportToExtImport(
        {
          kind: "refImport",
          importDefault: "LoginPage",
          from: "./LoginPage",
          sourceFilePath: `${projectRootDir}/src/auth/auth.wasp.ts`,
        },
        { projectRootDir },
      ),
    ).toStrictEqual({
      kind: "default",
      name: "LoginPage",
      path: "@src/auth/LoginPage",
    });
  });

  test("rejects relative paths without source file information", () => {
    expect(() =>
      mapRefImportToExtImport(
        { kind: "refImport", importDefault: "LoginPage", from: "./LoginPage" },
        { projectRootDir },
      ),
    ).toThrowError(SpecUserError);
  });

  test("rejects relative descriptors without the refImport marker", () => {
    expect(() =>
      mapRefImportToExtImport(
        {
          importDefault: "LoginPage",
          from: "./LoginPage",
          sourceFilePath: `${projectRootDir}/src/auth/auth.wasp.ts`,
        },
        { projectRootDir },
      ),
    ).toThrowError(SpecUserError);
  });

  test("rejects raw @src descriptors", () => {
    expect(() =>
      mapRefImportToExtImport(
        { importDefault: "LoginPage", from: "@src/LoginPage" },
        { projectRootDir },
      ),
    ).toThrowError(SpecUserError);
  });

  function testRefImportMapping(
    refImport: TsAppSpec.RefImport,
    expected: AppSpec.ExtImport,
  ): void {
    const result = mapRefImportToExtImport(refImport, { projectRootDir });

    expect(result).toStrictEqual(expected);
  }
});

describe("refImport", () => {
  test("marks a descriptor as a RefImport", () => {
    expect(
      refImport({ importDefault: "MainPage", from: "./MainPage" }),
    ).toStrictEqual({
      kind: "refImport",
      importDefault: "MainPage",
      from: "./MainPage",
    });
  });
});

describe("makeRefImport", () => {
  test("returns a source-aware refImport helper", () => {
    const sourceFilePath = "/project/main.wasp.ts";
    const sourceAwareRefImport = makeRefImport(
      pathToFileURL(sourceFilePath).href,
    );

    expect(
      sourceAwareRefImport({
        import: "archive",
        from: "./operations",
        alias: "archiveTask",
      }),
    ).toStrictEqual({
      kind: "refImport",
      import: "archive",
      from: "./operations",
      alias: "archiveTask",
      sourceFilePath,
    });
  });
});
