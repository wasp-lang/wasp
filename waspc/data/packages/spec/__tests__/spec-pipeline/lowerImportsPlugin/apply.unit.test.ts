import { describe, expect, test } from "vitest";
import { getLoweredImportSource } from "../../../src/spec-pipeline/lowerImportsPlugin/apply.js";

describe("getLoweredImportSource", () => {
  test("lowers a default import into a DefaultRefObject (importDefault)", () => {
    expect(
      getLoweredImportSource({
        kind: "default",
        refObject: {
          importDefault: "MainPage",
          from: "@src/MainPage",
        },
      }),
    ).toBe(
      `const MainPage = {"importDefault":"MainPage","from":"@src/MainPage"} as const;\n`,
    );
  });

  test("lowers a named import without an alias when local matches imported", () => {
    expect(
      getLoweredImportSource({
        kind: "named",
        refObject: {
          import: "getTasks",
          from: "@src/operations",
        },
      }),
    ).toBe(
      `const getTasks = {"import":"getTasks","from":"@src/operations"} as const;\n`,
    );
  });

  test("lowers an aliased named import carrying the alias", () => {
    expect(
      getLoweredImportSource({
        kind: "named",
        refObject: {
          import: "archive",
          alias: "archiveTask",
          from: "@src/operations",
        },
      }),
    ).toBe(
      `const archiveTask = {"import":"archive","alias":"archiveTask","from":"@src/operations"} as const;\n`,
    );
  });

  test("lowers a namespace import into a Proxy with a name-prefixed alias", () => {
    expect(
      getLoweredImportSource({
        kind: "namespace",
        alias: "ops",
        from: "@src/operations",
      }),
    ).toBe(
      `const ops = new Proxy({}, { get: (_t, k) => ({ import: String(k), from: "@src/operations", alias: "ops_" + String(k) } as const) }) as Record<string, { import: string; from: "@src/operations"; alias: string }>;\n`,
    );
  });
});
