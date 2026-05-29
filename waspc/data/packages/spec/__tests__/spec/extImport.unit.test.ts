import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import { mapExtImport } from "../../src/spec/refObject.js";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import { SpecUserError } from "../../src/spec/specUserError.js";
import * as Fixtures from "./testFixtures.js";

describe("mapExtImport", () => {
  test("should map minimal named import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("minimal", "named"));
  });

  test("should map full named import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("full", "named"));
  });

  test("should map minimal default import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("minimal", "default"));
  });

  test("should map full default import correctly", () => {
    testMapExtImport(Fixtures.getExtImport("full", "default"));
  });

  test.each([
    { extImport: () => null },
    { extImport: { parse: () => ({}) } },
    { extImport: { from: "@src/external" } },
  ])("returns an error for invalid runtime values", ({ extImport }) => {
    expect(() => mapExtImport(extImport)).toThrowError(SpecUserError);
    expect(() => mapExtImport(extImport)).toThrowError(
      "Got an import in the Wasp file that we couldn't process",
    );
  });

  function testMapExtImport(extImport: TsAppSpec.RefObject): void {
    const result = mapExtImport(extImport);

    if ("import" in extImport) {
      expect(result).toStrictEqual({
        kind: "named",
        name: extImport.import,
        path: extImport.from,
        alias: extImport.alias,
      } satisfies AppSpec.ExtImport);
    } else {
      expect(result).toStrictEqual({
        kind: "default",
        name: extImport.importDefault,
        path: extImport.from,
      } satisfies AppSpec.ExtImport);
    }
  }
});
