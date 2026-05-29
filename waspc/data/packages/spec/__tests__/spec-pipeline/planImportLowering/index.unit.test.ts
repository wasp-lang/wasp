import { describe, expect, test } from "vitest";
import { planImportLowering } from "../../../src/spec-pipeline/planImportLowering/index.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("planImportLowering", () => {
  const projectRootDir = "/project";
  const sourcePath = `${projectRootDir}/main.wasp.ts`;

  test("plans lowered bindings for supported ref import shapes", () => {
    const plan = planImportLowering({
      sourcePath,
      sourceText: [
        `import MainPage from "./src/MainPage" with { type: "ref" };`,
        `import { getTasks, archive as archiveTask } from "./src/operations" with { type: "ref" };`,
        `import * as ops from "./src/operations" with { type: "ref" };`,
        ``,
      ].join("\n"),
    });

    expect(
      plan.replacements.flatMap((replacement) => replacement.bindings),
    ).toEqual([
      {
        kind: "refImport",
        localName: "MainPage",
        descriptor: { importDefault: "MainPage", from: "./src/MainPage" },
      },
      {
        kind: "refImport",
        localName: "getTasks",
        descriptor: { import: "getTasks", from: "./src/operations" },
      },
      {
        kind: "refImport",
        localName: "archiveTask",
        descriptor: {
          import: "archive",
          from: "./src/operations",
          alias: "archiveTask",
        },
      },
      {
        kind: "namespace",
        localName: "ops",
        from: "./src/operations",
        aliasPrefix: "ops_",
      },
    ]);
  });

  test("preserves paths from import specifiers", () => {
    const plan = planImportLowering({
      sourcePath: `${projectRootDir}/src/features/home.wasp.ts`,
      sourceText: `import MainPage from "./MainPage" with { type: "ref" };\n`,
    });

    expect(plan.replacements[0]?.bindings).toEqual([
      {
        kind: "refImport",
        localName: "MainPage",
        descriptor: {
          importDefault: "MainPage",
          from: "./MainPage",
        },
      },
    ]);
  });

  test("leaves non-ref imports and re-exports out of the plan", () => {
    expect(
      planImportLowering({
        sourcePath,
        sourceText: [
          `import { app } from "@wasp.sh/spec";`,
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
      source: `import "./src/setup" with { type: "ref" };\n`,
      unsupportedImportType: "sideEffect",
      refImportPath: "./src/setup",
      expectedMessage: "Side-effect imports are not supported.",
    },
    {
      source: `import type { Props } from "./src/MainPage" with { type: "ref" };\n`,
      unsupportedImportType: "typeOnly",
      refImportPath: "./src/MainPage",
      expectedMessage: "Type-only imports are not supported.",
    },
    {
      source: `import { type Props, MainPage } from "./src/MainPage" with { type: "ref" };\n`,
      unsupportedImportType: "mixedTypeAndValue",
      refImportPath: "./src/MainPage",
      expectedMessage: "Mixed type/value imports are not supported.",
    },
    {
      source: `import { "foo-bar" as fooBar } from "./src/operations" with { type: "ref" };\n`,
      unsupportedImportType: "stringLiteral",
      refImportPath: "./src/operations",
      expectedMessage: "String-literal named imports are not supported.",
    },
    {
      source: `import {} from "./src/MainPage" with { type: "ref" };\n`,
      unsupportedImportType: "emptyNamed",
      refImportPath: "./src/MainPage",
      expectedMessage: "Empty named imports are not supported.",
    },
    {
      source: `export { MainPage } from "./src/MainPage" with { type: "ref" };\n`,
      unsupportedImportType: "reExport",
      refImportPath: "./src/MainPage",
      expectedMessage: "Re-exports are not supported.",
    },
  ])(
    "throws SpecUserError for $unsupportedImportType",
    ({ source, expectedMessage, refImportPath }) => {
      const getPlan = () =>
        planImportLowering({ sourceText: source, sourcePath });

      expect(getPlan).toThrowError(SpecUserError);
      expect(getPlan).toThrowError(
        `${sourcePath}(1,1): error: Unsupported ref import ${JSON.stringify(refImportPath)}. ${expectedMessage}`,
      );
    },
  );

  test("reports all unsupported ref imports", () => {
    const source = [
      `import "./src/setup" with { type: "ref" };`,
      `import type { Props } from "./src/MainPage" with { type: "ref" };`,
      ``,
    ].join("\n");
    const getPlan = () =>
      planImportLowering({ sourceText: source, sourcePath });

    expect(getPlan).toThrowError(SpecUserError);
    expect(getPlan).toThrowError(
      [
        `${sourcePath}(1,1): error: Unsupported ref import "./src/setup". Side-effect imports are not supported.`,
        `${sourcePath}(2,1): error: Unsupported ref import "./src/MainPage". Type-only imports are not supported.`,
        ``,
        `Supported ref imports are default, named, aliased named, or namespace imports marked with { type: "ref" }.`,
      ].join("\n"),
    );
  });
});
