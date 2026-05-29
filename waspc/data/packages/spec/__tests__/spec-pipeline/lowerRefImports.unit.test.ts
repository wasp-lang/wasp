import { describe, expect, test } from "vitest";
import { lowerRefImports } from "../../src/spec-pipeline/lowerRefImports.js";
import { SpecUserError } from "../../src/spec/specUserError.js";

describe("lowerRefImports", () => {
  const projectRootDir = "/project";
  const sourcePath = `${projectRootDir}/main.wasp.ts`;

  test("lowers a default ref import into a refImport const", () => {
    const input = `import MainPage from "./src/MainPage" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      [
        getExpectedRefImportSetupSource(),
        `const MainPage = refImport({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );
  });

  test("lowers named and aliased ref imports into refImport consts", () => {
    const input = `import { getTasks, archive as archiveTask } from "./src/operations" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      [
        getExpectedRefImportSetupSource(),
        `const getTasks = refImport({ import: "getTasks", from: "./src/operations" });`,
        `const archiveTask = refImport({ import: "archive", from: "./src/operations", alias: "archiveTask" });`,
        ``,
      ].join("\n"),
    );
  });

  test("lowers same exported name aliases from different modules", () => {
    const input = [
      `import { archive as archiveTask } from "./src/operations" with { type: "ref" };`,
      `import { archive as archiveLegacyTask } from "./src/legacyOperations" with { type: "ref" };`,
      ``,
    ].join("\n");

    expect(lower(input)).toBe(
      [
        getExpectedRefImportSetupSource(),
        `const archiveTask = refImport({ import: "archive", from: "./src/operations", alias: "archiveTask" });`,
        `const archiveLegacyTask = refImport({ import: "archive", from: "./src/legacyOperations", alias: "archiveLegacyTask" });`,
        ``,
      ].join("\n"),
    );
  });

  test("lowers multiple named ref imports into separate refImport consts", () => {
    const input = `import { getTasks, createTask } from "./src/operations" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      [
        getExpectedRefImportSetupSource(),
        `const getTasks = refImport({ import: "getTasks", from: "./src/operations" });`,
        `const createTask = refImport({ import: "createTask", from: "./src/operations" });`,
        ``,
      ].join("\n"),
    );
  });

  test("lowers a default + named ref import together", () => {
    const input = `import MainPage, { Helper } from "./src/MainPage" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      [
        getExpectedRefImportSetupSource(),
        `const MainPage = refImport({ importDefault: "MainPage", from: "./src/MainPage" });`,
        `const Helper = refImport({ import: "Helper", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );
  });

  test("lowers a namespace ref import into a Proxy", () => {
    const input = `import * as ops from "./src/operations" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      [
        getExpectedRefImportSetupSource(),
        getExpectedNamespaceProxySource("ops", "./src/operations", "ops_"),
        ``,
      ].join("\n"),
    );
  });

  test("lowers same exported name through different namespaces", () => {
    const input = [
      `import * as ops from "./src/operations" with { type: "ref" };`,
      `import * as legacyOps from "./src/legacyOperations" with { type: "ref" };`,
      ``,
    ].join("\n");

    expect(lower(input)).toBe(
      [
        getExpectedRefImportSetupSource(),
        getExpectedNamespaceProxySource("ops", "./src/operations", "ops_"),
        getExpectedNamespaceProxySource(
          "legacyOps",
          "./src/legacyOperations",
          "legacyOps_",
        ),
        ``,
      ].join("\n"),
    );
  });

  test("leaves non-ref imports and re-exports untouched", () => {
    const input = [
      `import { App } from "@wasp.sh/spec";`,
      `import z from "zod";`,
      `import helper from "./helpers";`,
      `import MainPage from "@src/MainPage";`,
      `export { helper } from "./helpers";`,
      ``,
    ].join("\n");

    expect(lower(input)).toBe(input);
  });

  test("lowers only ref imports in a mixed file", () => {
    const input = [
      `import { app } from "@wasp.sh/spec";`,
      `import MainPage from "./src/MainPage" with { type: "ref" };`,
      `import { getTasks } from "./src/operations" with { type: "ref" };`,
      `import helper from "./helpers";`,
      ``,
      `const demoApp = app({ name: "demo", title: "Demo", wasp: { version: "^0.16.0" }, decls: [] });`,
      `export default demoApp;`,
      ``,
    ].join("\n");
    const output = lower(input);

    expect(output).toContain(
      `const MainPage = refImport({ importDefault: "MainPage", from: "./src/MainPage" });`,
    );
    expect(output).toContain(
      `const getTasks = refImport({ import: "getTasks", from: "./src/operations" });`,
    );
    expect(output).toContain(
      `import { app, makeRefImport } from "@wasp.sh/spec";`,
    );
    expect(output).toContain(
      `const refImport = makeRefImport(import.meta.url);`,
    );
    expect(output).toContain(`import helper from "./helpers";`);
    expect(output).toContain(`export default demoApp;`);
    expect(output).not.toContain(`with { type: "ref" }`);
  });

  test("is a no-op on an ExtImport-form spec file", () => {
    const input = [
      `import { app } from "@wasp.sh/spec";`,
      ``,
      `const demoApp = app({ name: "demo", title: "Demo", wasp: { version: "^0.16.0" }, decls: [] });`,
      `demoApp.client = { rootComponent: { importDefault: "MainPage", from: "@src/MainPage" } };`,
      `export default demoApp;`,
      ``,
    ].join("\n");

    expect(lower(input)).toBe(input);
  });

  test("rewrites explicit refImport imports", () => {
    const input = [
      `import { app, refImport } from "@wasp.sh/spec";`,
      `const MainPage = refImport({ importDefault: "MainPage", from: "./MainPage" });`,
      ``,
    ].join("\n");

    expect(lower(input)).toBe(
      [
        `import { app, makeRefImport } from "@wasp.sh/spec";`,
        `const refImport = makeRefImport(import.meta.url);`,
        `const MainPage = refImport({ importDefault: "MainPage", from: "./MainPage" });`,
        ``,
      ].join("\n"),
    );
  });

  test.each([
    {
      source: `import "./src/setup" with { type: "ref" };\n`,
      expectedMessage: "Side-effect imports are not supported.",
    },
    {
      source: `import type { Props } from "./src/MainPage" with { type: "ref" };\n`,
      expectedMessage: "Type-only imports are not supported.",
    },
    {
      source: `import { type Props, MainPage } from "./src/MainPage" with { type: "ref" };\n`,
      expectedMessage: "Mixed type/value imports are not supported.",
    },
    {
      source: `import { "foo-bar" as fooBar } from "./src/operations" with { type: "ref" };\n`,
      expectedMessage: "String-literal named imports are not supported.",
    },
    {
      source: `import {} from "./src/MainPage" with { type: "ref" };\n`,
      expectedMessage: "Empty named imports are not supported.",
    },
    {
      source: `export { MainPage } from "./src/MainPage" with { type: "ref" };\n`,
      expectedMessage: "Re-exports are not supported.",
    },
  ])("rejects unsupported ref import forms", ({ source, expectedMessage }) => {
    expectSpecUserError(source, expectedMessage);
  });

  test("reports all unsupported ref imports", () => {
    const input = [
      `import "./src/setup" with { type: "ref" };`,
      `import type { MainPageProps } from "./src/MainPage" with { type: "ref" };`,
      ``,
    ].join("\n");

    expectSpecUserError(
      input,
      [
        `${sourcePath}(1,1): error: Unsupported ref import "./src/setup". Side-effect imports are not supported.`,
        `${sourcePath}(2,1): error: Unsupported ref import "./src/MainPage". Type-only imports are not supported.`,
        ``,
        `Supported ref imports are default, named, aliased named, or namespace imports marked with { type: "ref" }.`,
      ].join("\n"),
    );
  });

  function lower(sourceText: string): string {
    return lowerRefImports({ sourceText, sourcePath, projectRootDir });
  }
});

function getExpectedNamespaceProxySource(
  localName: string,
  from: string,
  aliasPrefix: string,
): string {
  const quotedFrom = JSON.stringify(from);
  const quotedAliasPrefix = JSON.stringify(aliasPrefix);

  return `const ${localName} = new Proxy({}, { get: (_t, k) => refImport({ import: String(k), from: ${quotedFrom}, alias: ${quotedAliasPrefix} + String(k) }) }) as Record<string, ReturnType<typeof refImport>>;`;
}

function getExpectedRefImportSetupSource(): string {
  return [
    `import { makeRefImport } from "@wasp.sh/spec";`,
    `const refImport = makeRefImport(import.meta.url);`,
  ].join("\n");
}

function expectSpecUserError(
  sourceText: string,
  expectedMessage: string | RegExp,
): void {
  const projectRootDir = "/project";
  const sourcePath = `${projectRootDir}/main.wasp.ts`;
  const getLoweredSource = () =>
    lowerRefImports({ sourceText, sourcePath, projectRootDir });

  expect(getLoweredSource).toThrowError(SpecUserError);
  expect(getLoweredSource).toThrowError(expectedMessage);
}
