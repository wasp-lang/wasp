import { describe, expect, test } from "vitest";
import {
  AuthoredExtImport,
  formatExtImportDescriptorError,
  mapAuthoredExtImport,
} from "../../src/appSpec/extImportDescriptor.js";

describe("mapAuthoredExtImport", () => {
  test("maps named descriptors with alias metadata", () => {
    expect(
      mapAuthoredExtImport({
        import: "archive",
        from: "@src/operations",
        alias: "archiveTask",
      }),
    ).toEqual({
      status: "ok",
      value: {
        kind: "named",
        name: "archive",
        path: "@src/operations",
        alias: "archiveTask",
      },
    });
  });

  test("maps default descriptors", () => {
    expect(
      mapAuthoredExtImport({
        importDefault: "MainPage",
        from: "@src/MainPage",
      }),
    ).toEqual({
      status: "ok",
      value: {
        kind: "default",
        name: "MainPage",
        path: "@src/MainPage",
      },
    });
  });

  test.each([
    {
      extImport: (() => null) as AuthoredExtImport,
      reason: "functionValue",
      message: /got a function at runtime/,
    },
    {
      extImport: { import: "archive", from: "@src/operations", alias: 42 },
      reason: "descriptorLikeObject",
      message: /Invalid ExtImport descriptor/,
    },
    {
      extImport: { parse: () => ({}) },
      reason: "objectValue",
      message: /got an object at runtime/,
    },
  ])("returns $reason for invalid values", ({ extImport, reason, message }) => {
    const result = mapAuthoredExtImport(extImport as AuthoredExtImport);

    expect(result).toEqual({ status: "error", reason });
    if (result.status !== "error") return;
    expect(formatExtImportDescriptorError(result.reason)).toMatch(message);
  });
});
