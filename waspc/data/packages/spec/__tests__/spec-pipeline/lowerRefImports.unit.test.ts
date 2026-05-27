import { describe, expect, test } from "vitest";
import { lowerRefImports } from "../../src/spec-pipeline/lowerRefImports.js";
import { SpecUserError } from "../../src/spec/specUserError.js";

describe("lowerRefImports", () => {
  const projectRootDir = "/project";
  const sourcePath = `${projectRootDir}/main.wasp.ts`;

  test("lowers a default ref import into an importDefault ExtImport const", () => {
    const input = `import MainPage from "./src/MainPage" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;\n`,
    );
  });

  test("lowers named and aliased ref imports into ExtImport consts", () => {
    const input = `import { getTasks, archive as archiveTask } from "./src/operations" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      `const getTasks = { import: "getTasks", from: "@src/operations" } as const;\nconst archiveTask = { import: "archive", from: "@src/operations", alias: "archiveTask" } as const;\n`,
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
        `const archiveTask = { import: "archive", from: "@src/operations", alias: "archiveTask" } as const;`,
        `const archiveLegacyTask = { import: "archive", from: "@src/legacyOperations", alias: "archiveLegacyTask" } as const;`,
        ``,
      ].join("\n"),
    );
  });

  test("lowers multiple named ref imports into separate ExtImport consts", () => {
    const input = `import { getTasks, createTask } from "./src/operations" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      `const getTasks = { import: "getTasks", from: "@src/operations" } as const;\nconst createTask = { import: "createTask", from: "@src/operations" } as const;\n`,
    );
  });

  test("lowers a default + named ref import together", () => {
    const input = `import MainPage, { Helper } from "./src/MainPage" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;\nconst Helper = { import: "Helper", from: "@src/MainPage" } as const;\n`,
    );
  });

  test("lowers a namespace ref import into a Proxy", () => {
    const input = `import * as ops from "./src/operations" with { type: "ref" };\n`;

    expect(lower(input)).toBe(
      `${expectedNamespaceProxy("ops", "@src/operations", "ops_")}\n`,
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
        expectedNamespaceProxy("ops", "@src/operations", "ops_"),
        expectedNamespaceProxy(
          "legacyOps",
          "@src/legacyOperations",
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
      `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;`,
    );
    expect(output).toContain(
      `const getTasks = { import: "getTasks", from: "@src/operations" } as const;`,
    );
    expect(output).toContain(`import { app } from "@wasp.sh/spec";`);
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

function expectedNamespaceProxy(
  localName: string,
  from: string,
  aliasPrefix: string,
): string {
  const quotedFrom = JSON.stringify(from);
  const quotedAliasPrefix = JSON.stringify(aliasPrefix);

  return `const ${localName} = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: ${quotedFrom}, alias: ${quotedAliasPrefix} + String(k) } as const) }) as Record<string, { import: string; from: ${quotedFrom}; alias: string }>;`;
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
