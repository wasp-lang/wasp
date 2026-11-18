import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import type * as WaspSpec from "../../src/spec/publicApi/waspSpec.js";
import { mapRefObject } from "../../src/spec/refObject.js";
import { WaspSpecUserError } from "../../src/spec/waspSpecUserError.js";
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
    expect(() => mapRefObjectForProject(refObject)).toThrow(WaspSpecUserError);
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
        path: "@src/external",
        alias: refObject.alias,
      } satisfies AppSpec.ExtImport);
    } else {
      expect(result).toStrictEqual({
        kind: "default",
        name: refObject.importDefault,
        path: "@src/external",
      } satisfies AppSpec.ExtImport);
    }
  }

  function mapRefObjectForProject(refObject: unknown): AppSpec.ExtImport {
    return mapRefObject(refObject, {
      projectRootDir: Fixtures.MOCK_PROJECT_DIR,
    });
  }
});
