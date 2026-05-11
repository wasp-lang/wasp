import assert from "node:assert";
import { describe, expect, test } from "vitest";
import { planImportLowering } from "../../../src/spec-pipeline/planImportLowering/index.js";

describe("planImportLowering", () => {
  test("plans lowered bindings for supported import shapes", () => {
    const result = planImportLowering(
      [
        `import MainPage from "@src/MainPage";`,
        `import { getTasks, archive as archiveTask } from "@src/operations";`,
        `import * as ops from "@src/operations";`,
        ``,
      ].join("\n"),
    );

    assert(result.status === "ok");

    expect(
      result.value.replacements.flatMap((replacement) => replacement.bindings),
    ).toEqual([
      {
        kind: "extImport",
        localName: "MainPage",
        extImport: { importDefault: "MainPage", from: "@src/MainPage" },
      },
      {
        kind: "extImport",
        localName: "getTasks",
        extImport: { import: "getTasks", from: "@src/operations" },
      },
      {
        kind: "extImport",
        localName: "archiveTask",
        extImport: {
          import: "archive",
          from: "@src/operations",
          alias: "archiveTask",
        },
      },
      {
        kind: "namespace",
        localName: "ops",
        from: "@src/operations",
        aliasPrefix: "ops_",
      },
    ]);
  });

  test("leaves package imports out of the plan", () => {
    const result = planImportLowering(`import { App } from "wasp-config";\n`);

    expect(result).toEqual({ status: "ok", value: { replacements: [] } });
  });

  test("leaves non-@src imports and re-exports out of the plan", () => {
    const result = planImportLowering(
      [
        `import helper from "./helpers";`,
        `import MainPage from "./src/MainPage";`,
        `export { helper } from "./helpers";`,
        ``,
      ].join("\n"),
    );

    expect(result).toEqual({ status: "ok", value: { replacements: [] } });
  });

  test.each([
    {
      source: `import "@src/setup";\n`,
      unsupportedImportType: "sideEffect",
      specifier: "@src/setup",
    },
    {
      source: `import MainPage = require("@src/MainPage");\n`,
      unsupportedImportType: "importEquals",
      specifier: "@src/MainPage",
    },
    {
      source: `import type { Props } from "@src/MainPage";\n`,
      unsupportedImportType: "typeOnly",
      specifier: "@src/MainPage",
    },
    {
      source: `import { type Props, MainPage } from "@src/MainPage";\n`,
      unsupportedImportType: "mixedTypeAndValue",
      specifier: "@src/MainPage",
    },
    {
      source: `import { "foo-bar" as fooBar } from "@src/operations";\n`,
      unsupportedImportType: "stringLiteral",
      specifier: "@src/operations",
    },
    {
      source: `import {} from "@src/MainPage";\n`,
      unsupportedImportType: "emptyNamed",
      specifier: "@src/MainPage",
    },
    {
      source: `export { MainPage } from "@src/MainPage";\n`,
      unsupportedImportType: "reExport",
      specifier: "@src/MainPage",
    },
  ])(
    "returns a diagnostic for $unsupportedImportType",
    ({ source, unsupportedImportType, specifier }) => {
      const result = planImportLowering(source);

      assert(result.status === "error");

      expect(result.error).toEqual([
        {
          unsupportedImportType,
          specifier,
          location: { line: 1, column: 1 },
        },
      ]);
    },
  );
});
