import { describe, expect, test } from "vitest";
import type * as AppSpec from "../../src/appSpec.js";
import type * as TsAppSpec from "../../src/spec/publicApi/tsAppSpec.js";
import { mapReferenceObject } from "../../src/spec/referenceObject.js";
import { SpecUserError } from "../../src/spec/specUserError.js";
import * as Fixtures from "./testFixtures.js";

describe("mapReferenceObject", () => {
  test("should map minimal named reference correctly", () => {
    testMapReferenceObject(Fixtures.getReferenceObject("minimal", "named"));
  });

  test("should map full named reference correctly", () => {
    testMapReferenceObject(Fixtures.getReferenceObject("full", "named"));
  });

  test("should map default reference correctly", () => {
    testMapReferenceObject(Fixtures.getReferenceObject("full", "default"));
  });

  test("should throw when default reference has no alias", () => {
    const ref = { import: "default", from: "@src/external" };

    expect(() => mapReferenceObject(ref)).toThrowError(SpecUserError);
    expect(() => mapReferenceObject(ref)).toThrowError(
      /Default imports must include an `alias`/,
    );
  });

  test.each([
    { ref: () => null },
    { ref: { parse: () => ({}) } },
    { ref: { from: "@src/external" } },
  ])("returns an error for invalid runtime values", ({ ref }) => {
    expect(() => mapReferenceObject(ref)).toThrowError(SpecUserError);
    expect(() => mapReferenceObject(ref)).toThrowError(
      "Got a reference in the Wasp file that we couldn't process",
    );
  });

  function testMapReferenceObject(ref: TsAppSpec.ReferenceObject): void {
    const result = mapReferenceObject(ref);

    if (ref.import === "default") {
      if (ref.alias === undefined) {
        throw new Error("Default reference fixture must have an alias");
      }
      expect(result).toStrictEqual({
        kind: "default",
        name: ref.alias,
        path: ref.from,
      } satisfies AppSpec.ExtImport);
    } else {
      expect(result).toStrictEqual({
        kind: "named",
        name: ref.import,
        path: ref.from,
        alias: ref.alias,
      } satisfies AppSpec.ExtImport);
    }
  }
});
