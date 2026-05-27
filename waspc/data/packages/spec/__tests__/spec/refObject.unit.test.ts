import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import { isDefaultRefObject, mapRefObject } from "../../src/spec/refObject.js";
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
    { refObject: { from: "@src/external" } },
  ])("returns an error for invalid runtime values", ({ refObject }) => {
    expect(() => mapRefObject(refObject)).toThrowError(SpecUserError);
    expect(() => mapRefObject(refObject)).toThrowError(
      "Got an import in the Wasp file that we couldn't process",
    );
  });

  function testMapRefObject(refObject: TsAppSpec.RefObject): void {
    const result = mapRefObject(refObject);

    if (isDefaultRefObject(refObject)) {
      expect(result).toStrictEqual({
        kind: "default",
        name: refObject.alias,
        path: refObject.from,
      } satisfies AppSpec.ExtImport);
    } else {
      expect(result).toStrictEqual({
        kind: "named",
        name: refObject.import,
        path: refObject.from,
        alias: refObject.alias,
      } satisfies AppSpec.ExtImport);
    }
  }
});
