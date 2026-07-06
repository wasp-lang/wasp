import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import type * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";
import { mapRefObject } from "../../src/spec/refObject.js";
import { SpecUserError } from "../../src/spec/specUserError.js";
import * as Fixtures from "./testFixtures.js";

describe("mapRefObject", () => {
  test("should map minimal named import correctly", () => {
    testMapRefObject(Fixtures.getRefObject("minimal", "named"));
  });

  test("should map full named import correctly", () => {
    testMapRefObject(Fixtures.getRefObject("full", "named"));
  });

  test("should map minimal default import correctly", () => {
    testMapRefObject(Fixtures.getRefObject("minimal", "default"));
  });

  test("should map full default import correctly", () => {
    testMapRefObject(Fixtures.getRefObject("full", "default"));
  });

  test.each([
    { refObject: () => null },
    { refObject: { parse: () => ({}) } },
    { refObject: { from: "./src/external" } },
  ])("returns an error for invalid runtime values", ({ refObject }) => {
    expect(() => mapRefObjectForProject(refObject)).toThrow(SpecUserError);
    expect(() => mapRefObjectForProject(refObject)).toThrow(
      "Got an import in the Wasp file that we couldn't process",
    );
  });

  function testMapRefObject(refObject: WaspSpec.RefObject): void {
    const result = mapRefObjectForProject(refObject);

    if ("import" in refObject) {
      expect(result).toStrictEqual({
        kind: "named",
        name: refObject.import,
        source: { kind: "project-src", path: "@src/external" },
        alias: refObject.alias,
      } satisfies AppSpec.ExtImport);
    } else {
      expect(result).toStrictEqual({
        kind: "default",
        name: refObject.importDefault,
        source: { kind: "project-src", path: "@src/external" },
      } satisfies AppSpec.ExtImport);
    }
  }

  test("should map named package import correctly", () => {
    expect(
      mapRefObjectForProject(
        Fixtures.getRefObjectForMockProject({
          import: "SkateboardPage",
          alias: "SkateboardPageAlias",
          from: "@skateboard/fsm/SkateboardPage",
        }),
      ),
    ).toStrictEqual({
      kind: "named",
      name: "SkateboardPage",
      alias: "SkateboardPageAlias",
      source: {
        kind: "package",
        packageName: "@skateboard/fsm",
        subpath: "SkateboardPage",
      },
    } satisfies AppSpec.ExtImport);
  });

  test("should map default package import correctly", () => {
    expect(
      mapRefObjectForProject(
        Fixtures.getRefObjectForMockProject({
          importDefault: "DefaultExport",
          from: "unscoped-package/nested/export",
        }),
      ),
    ).toStrictEqual({
      kind: "default",
      name: "DefaultExport",
      source: {
        kind: "package",
        packageName: "unscoped-package",
        subpath: "nested/export",
      },
    } satisfies AppSpec.ExtImport);
  });

  test.each(["/abs/path", "C:\\abs\\path", "\\\\server\\share\\path"])(
    "should reject absolute ref path %s",
    (from) => {
      expect(() =>
        mapRefObjectForProject(
          Fixtures.getRefObjectForMockProject({
            importDefault: "DefaultExport",
            from,
          }),
        ),
      ).toThrow(SpecUserError);
      expect(() =>
        mapRefObjectForProject(
          Fixtures.getRefObjectForMockProject({
            importDefault: "DefaultExport",
            from,
          }),
        ),
      ).toThrow("Absolute ref paths are not supported");
    },
  );

  test("should reject scoped package refs without package name", () => {
    expect(() =>
      mapRefObjectForProject(
        Fixtures.getRefObjectForMockProject({
          importDefault: "DefaultExport",
          from: "@scope",
        }),
      ),
    ).toThrow(SpecUserError);
    expect(() =>
      mapRefObjectForProject(
        Fixtures.getRefObjectForMockProject({
          importDefault: "DefaultExport",
          from: "@scope",
        }),
      ),
    ).toThrow("must include both scope and package name");
  });

  function mapRefObjectForProject(refObject: unknown): AppSpec.ExtImport {
    return mapRefObject(refObject, {
      projectRootDir: Fixtures.MOCK_PROJECT_DIR,
    });
  }
});
