import { pathToFileURL } from "node:url";
import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import {
  makeRefImport,
  mapExtImport,
  refImport,
} from "../../src/spec/refImport.js";
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

  function testMapExtImport(extImport: TsAppSpec.ExtImport): void {
    // TODO: Remove raw ExtImport coverage after reference import lowering emits
    // refImport(...) calls instead of plain descriptors.
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

describe("refImport", () => {
  test("marks a descriptor as a RefImport", () => {
    expect(
      refImport({ importDefault: "MainPage", from: "@src/MainPage" }),
    ).toStrictEqual({
      kind: "refImport",
      importDefault: "MainPage",
      from: "@src/MainPage",
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
        from: "@src/operations",
        alias: "archiveTask",
      }),
    ).toStrictEqual({
      kind: "refImport",
      import: "archive",
      from: "@src/operations",
      alias: "archiveTask",
      sourceFilePath,
    });
  });
});
