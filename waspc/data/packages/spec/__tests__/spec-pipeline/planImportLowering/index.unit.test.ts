import { describe, expect, test } from "vitest";
import { planImportLowering } from "../../../src/spec-pipeline/planImportLowering/index.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("planImportLowering", () => {
  const sourcePath = "app/main.wasp.ts";

  test("plans lowered bindings for supported import shapes", () => {
    const plan = planImportLowering({
      sourcePath,
      sourceText: [
        `import MainPage from "@src/MainPage";`,
        `import { getTasks, archive as archiveTask } from "@src/operations";`,
        `import * as ops from "@src/operations";`,
        ``,
      ].join("\n"),
    });

    expect(
      plan.replacements.flatMap((replacement) => replacement.bindings),
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
    expect(
      planImportLowering({
        sourcePath,
        sourceText: `import { App } from "@wasp.sh/spec";\n`,
      }),
    ).toEqual({ replacements: [] });
  });

  test("leaves non-@src imports and re-exports out of the plan", () => {
    expect(
      planImportLowering({
        sourcePath,
        sourceText: [
          `import helper from "./helpers";`,
          `import MainPage from "./src/MainPage";`,
          `export { helper } from "./helpers";`,
          ``,
        ].join("\n"),
      }),
    ).toEqual({ replacements: [] });
  });

  test.each([
    {
      source: `import "@src/setup";\n`,
      unsupportedImportType: "sideEffect",
      specifier: "@src/setup",
      expectedMessage: "Side-effect imports are not supported.",
    },
    {
      source: `import MainPage = require("@src/MainPage");\n`,
      unsupportedImportType: "importEquals",
      specifier: "@src/MainPage",
      expectedMessage: "Import equals declarations are not supported.",
    },
    {
      source: `import type { Props } from "@src/MainPage";\n`,
      unsupportedImportType: "typeOnly",
      specifier: "@src/MainPage",
      expectedMessage: "Type-only imports are not supported.",
    },
    {
      source: `import { type Props, MainPage } from "@src/MainPage";\n`,
      unsupportedImportType: "mixedTypeAndValue",
      specifier: "@src/MainPage",
      expectedMessage: "Mixed type/value imports are not supported.",
    },
    {
      source: `import { "foo-bar" as fooBar } from "@src/operations";\n`,
      unsupportedImportType: "stringLiteral",
      specifier: "@src/operations",
      expectedMessage: "String-literal named imports are not supported.",
    },
    {
      source: `import {} from "@src/MainPage";\n`,
      unsupportedImportType: "emptyNamed",
      specifier: "@src/MainPage",
      expectedMessage: "Empty named imports are not supported.",
    },
    {
      source: `export { MainPage } from "@src/MainPage";\n`,
      unsupportedImportType: "reExport",
      specifier: "@src/MainPage",
      expectedMessage: "Re-exports are not supported.",
    },
  ])(
    "throws SpecUserError for $unsupportedImportType",
    ({ source, expectedMessage, specifier }) => {
      const getPlan = () =>
        planImportLowering({ sourceText: source, sourcePath });

      expect(getPlan).toThrowError(SpecUserError);
      expect(getPlan).toThrowError(
        `${sourcePath}(1,1): error: Unsupported @src import ${JSON.stringify(specifier)}. ${expectedMessage}`,
      );
    },
  );

  test("reports all unsupported imports from @src", () => {
    const source = [
      `import "@src/setup";`,
      `import type { Props } from "@src/MainPage";`,
      ``,
    ].join("\n");
    const getPlan = () =>
      planImportLowering({ sourceText: source, sourcePath });

    expect(getPlan).toThrowError(SpecUserError);
    expect(getPlan).toThrowError(
      [
        `app/main.wasp.ts(1,1): error: Unsupported @src import "@src/setup". Side-effect imports are not supported.`,
        `app/main.wasp.ts(2,1): error: Unsupported @src import "@src/MainPage". Type-only imports are not supported.`,
        ``,
        `Supported @src imports are default, named, aliased named, or namespace imports from @src/*.`,
      ].join("\n"),
    );
  });
});
