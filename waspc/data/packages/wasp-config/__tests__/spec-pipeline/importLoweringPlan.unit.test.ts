import { describe, expect, test } from "vitest";
import type { ImportLoweringResult } from "../../src/spec-pipeline/importLoweringPlan.js";
import { planImportLowering } from "../../src/spec-pipeline/importLoweringPlan.js";

describe("planImportLowering", () => {
  test("plans descriptor declarations for supported import shapes", () => {
    const result = planImportLowering(
      [
        `import MainPage from "@src/MainPage";`,
        `import { getTasks, archive as archiveTask } from "@src/operations";`,
        `import * as ops from "@src/operations";`,
        ``,
      ].join("\n"),
    );

    expectOkResult(result);

    expect(
      result.plan.replacements.flatMap(
        (replacement) => replacement.declarations,
      ),
    ).toEqual([
      {
        kind: "descriptor",
        localName: "MainPage",
        descriptor: { importDefault: "MainPage", from: "@src/MainPage" },
      },
      {
        kind: "descriptor",
        localName: "getTasks",
        descriptor: { import: "getTasks", from: "@src/operations" },
      },
      {
        kind: "descriptor",
        localName: "archiveTask",
        descriptor: {
          import: "archive",
          from: "@src/operations",
          alias: "archiveTask",
        },
      },
      {
        kind: "namespace",
        localName: "ops",
        descriptorPath: "@src/operations",
        aliasPrefix: "ops_",
      },
    ]);
  });

  test("leaves package imports out of the plan", () => {
    const result = planImportLowering(`import { App } from "wasp-config";\n`);

    expect(result).toEqual({ status: "ok", plan: { replacements: [] } });
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

    expect(result).toEqual({ status: "ok", plan: { replacements: [] } });
  });

  test.each([
    {
      source: `import "@src/setup";\n`,
      reason: "sideEffectImport",
      specifier: "@src/setup",
    },
    {
      source: `import type { Props } from "@src/MainPage";\n`,
      reason: "typeOnlyImport",
      specifier: "@src/MainPage",
    },
    {
      source: `import { type Props, MainPage } from "@src/MainPage";\n`,
      reason: "mixedTypeAndValueImport",
      specifier: "@src/MainPage",
    },
    {
      source: `import {} from "@src/MainPage";\n`,
      reason: "emptyNamedImport",
      specifier: "@src/MainPage",
    },
    {
      source: `export { MainPage } from "@src/MainPage";\n`,
      reason: "srcReExport",
      specifier: "@src/MainPage",
    },
  ])("returns a diagnostic for $reason", ({ source, reason, specifier }) => {
    const result = planImportLowering(source);

    expectErrorResult(result);

    expect(result.diagnostics).toEqual([
      {
        reason,
        specifier,
        location: { line: 1, column: 1 },
      },
    ]);
  });
});

function expectOkResult(
  result: ImportLoweringResult,
): asserts result is Extract<ImportLoweringResult, { status: "ok" }> {
  expect(result.status).toBe("ok");
}

function expectErrorResult(
  result: ImportLoweringResult,
): asserts result is Extract<ImportLoweringResult, { status: "error" }> {
  expect(result.status).toBe("error");
}
