import assert from "node:assert";
import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import { mapExtImport, tryMapExtImport } from "../../src/spec/extImport.js";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
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
    const result = tryMapExtImport(extImport);

    assert(result.status === "error");
    expect(result.error).toContain(
      "Got an import in the Wasp file that we couldn't process",
    );
    expect(() => mapExtImport(extImport)).toThrowError(result.error);
  });

  function testMapExtImport(extImport: TsAppSpec.ExtImport): void {
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
