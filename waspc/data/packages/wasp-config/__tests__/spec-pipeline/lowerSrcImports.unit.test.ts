import { describe, expect, test } from "vitest";
import { lowerSrcImports } from "../../src/spec-pipeline/lowerSrcImports.js";

describe("lowerSrcImports", () => {
  test("lowers a default import into an importDefault descriptor", () => {
    const input = `import MainPage from "@src/MainPage";\n`;
    const output = lowerSrcImports(input);
    expect(output).toBe(
      `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;\n`,
    );
  });

  test("lowers a single named import into an import descriptor", () => {
    const input = `import { getTasks } from "@src/operations";\n`;
    const output = lowerSrcImports(input);
    expect(output).toBe(
      `const getTasks = { import: "getTasks", from: "@src/operations" } as const;\n`,
    );
  });

  test("lowers an aliased named import with alias metadata", () => {
    const input = `import { archive as archiveTask } from "@src/operations";\n`;
    const output = lowerSrcImports(input);
    expect(output).toBe(
      `const archiveTask = { import: "archive", from: "@src/operations", alias: "archiveTask" } as const;\n`,
    );
  });

  test("lowers same exported name aliases from different modules", () => {
    const input = [
      `import { archive as archiveTask } from "@src/operations";`,
      `import { archive as archiveLegacyTask } from "@src/legacyOperations";`,
      ``,
    ].join("\n");
    const output = lowerSrcImports(input);
    expect(output).toBe(
      [
        `const archiveTask = { import: "archive", from: "@src/operations", alias: "archiveTask" } as const;`,
        `const archiveLegacyTask = { import: "archive", from: "@src/legacyOperations", alias: "archiveLegacyTask" } as const;`,
        ``,
      ].join("\n"),
    );
  });

  test("lowers multiple named imports into separate descriptor consts", () => {
    const input = `import { getTasks, createTask } from "@src/operations";\n`;
    const output = lowerSrcImports(input);
    expect(output).toBe(
      `const getTasks = { import: "getTasks", from: "@src/operations" } as const;\nconst createTask = { import: "createTask", from: "@src/operations" } as const;\n`,
    );
  });

  test("lowers a default + named import together", () => {
    const input = `import MainPage, { Helper } from "@src/MainPage";\n`;
    const output = lowerSrcImports(input);
    expect(output).toBe(
      `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;\nconst Helper = { import: "Helper", from: "@src/MainPage" } as const;\n`,
    );
  });

  test("lowers a namespace import into a Proxy", () => {
    const input = `import * as ops from "@src/operations";\n`;
    const output = lowerSrcImports(input);
    expect(output).toBe(
      `const ops = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: "@src/operations", alias: "ops_" + String(k) } as const) }) as Record<string, { import: string; from: "@src/operations"; alias: string }>;\n`,
    );
  });

  test("lowers same exported name through different namespaces", () => {
    const input = [
      `import * as ops from "@src/operations";`,
      `import * as legacyOps from "@src/legacyOperations";`,
      ``,
    ].join("\n");
    const output = lowerSrcImports(input);
    expect(output).toBe(
      [
        `const ops = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: "@src/operations", alias: "ops_" + String(k) } as const) }) as Record<string, { import: string; from: "@src/operations"; alias: string }>;`,
        `const legacyOps = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: "@src/legacyOperations", alias: "legacyOps_" + String(k) } as const) }) as Record<string, { import: string; from: "@src/legacyOperations"; alias: string }>;`,
        ``,
      ].join("\n"),
    );
  });

  test("leaves wasp-config imports untouched", () => {
    const input = `import { App } from "wasp-config";\n`;
    expect(lowerSrcImports(input)).toBe(input);
  });

  test("leaves package imports untouched", () => {
    const input = `import z from "zod";\nimport { App } from "wasp-config";\n`;
    expect(lowerSrcImports(input)).toBe(input);
  });

  test("leaves relative imports untouched", () => {
    const input = `import helper from "./helpers";\n`;

    expect(lowerSrcImports(input)).toBe(input);
  });

  test("leaves relative re-exports untouched", () => {
    const input = `export { helper } from "./helpers";\n`;

    expect(lowerSrcImports(input)).toBe(input);
  });

  test("leaves ./src imports untouched", () => {
    const input = `import MainPage from "./src/MainPage";\n`;

    expect(lowerSrcImports(input)).toBe(input);
  });

  test("lowers only the matching import in a mixed file", () => {
    const input = [
      `import { App } from "wasp-config";`,
      `import MainPage from "@src/MainPage";`,
      `import { getTasks } from "@src/operations";`,
      `import * as ops from "@src/operations";`,
      `import helper from "./helpers";`,
      ``,
      `const app = new App("demo", { title: "Demo", wasp: { version: "^0.16.0" } });`,
      `app.page("MainPage", { component: MainPage });`,
      `app.query("getTasks", { fn: getTasks });`,
      `app.action("logout", { fn: ops.logout });`,
      `export default app;`,
      ``,
    ].join("\n");
    const output = lowerSrcImports(input);
    expect(output).toContain(
      `const MainPage = { importDefault: "MainPage", from: "@src/MainPage" } as const;`,
    );
    expect(output).toContain(
      `const getTasks = { import: "getTasks", from: "@src/operations" } as const;`,
    );
    expect(output).toContain(
      `const ops = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: "@src/operations", alias: "ops_" + String(k) } as const) }) as Record<string, { import: string; from: "@src/operations"; alias: string }>;`,
    );
    expect(output).toContain(`import { App } from "wasp-config";`);
    expect(output).toContain(`import helper from "./helpers";`);
    expect(output).toContain(`export default app;`);
    expect(output).not.toMatch(/^import\s+(?:.+\s+from\s+)?["']@src\//m);
  });

  test("is a no-op on a descriptor-form spec file", () => {
    const input = [
      `import { App } from "wasp-config";`,
      ``,
      `const app = new App("demo", { title: "Demo", wasp: { version: "^0.16.0" } });`,
      `app.page("MainPage", { component: { import: "MainPage", from: "@src/MainPage" } });`,
      `export default app;`,
      ``,
    ].join("\n");
    expect(lowerSrcImports(input)).toBe(input);
  });

  test("rejects side-effect imports from @src", () => {
    const input = `import "@src/setup";\n`;

    expect(() => lowerSrcImports(input)).toThrowError(/Side-effect imports/);
  });

  test("rejects re-exports from @src", () => {
    const input = `export { MainPage } from "@src/MainPage";\n`;

    expect(() => lowerSrcImports(input)).toThrowError(/Re-exports/);
  });

  test("rejects type-only imports from @src", () => {
    const input = `import type { MainPageProps } from "@src/MainPage";\n`;

    expect(() => lowerSrcImports(input)).toThrowError(/Type-only imports/);
  });

  test("rejects mixed type and value imports from @src", () => {
    const input = `import { type MainPageProps, MainPage } from "@src/MainPage";\n`;

    expect(() => lowerSrcImports(input)).toThrowError(/Mixed type\/value imports/);
  });

  test("rejects empty named imports from @src", () => {
    const input = `import {} from "@src/MainPage";\n`;

    expect(() => lowerSrcImports(input)).toThrowError(/Empty named imports/);
  });
});
