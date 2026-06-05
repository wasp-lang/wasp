import { RolldownMagicString } from "rolldown";
import { describe, expect, test } from "vitest";
import {
  applyLowerImportsPlan_mutate,
  getLoweredImportSource,
} from "../../../src/spec-pipeline/lowerImportsPlugin/apply.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

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

  test("rejects a namespace import with a SpecUserError", () => {
    expect(() =>
      getLoweredImportSource({
        kind: "namespace",
        alias: "ops",
        from: "@src/operations",
      }),
    ).toThrow(SpecUserError);
  });

  test("mentions the offending namespace binding in the error", () => {
    expect(() =>
      getLoweredImportSource({
        kind: "namespace",
        alias: "ops",
        from: "@src/operations",
      }),
    ).toThrow(/import \* as ops/);
  });
});

describe("applyLowerImportsPlan_mutate", () => {
  test("rejects a plan containing a namespace import with a SpecUserError", () => {
    const magicString = new RolldownMagicString("");

    expect(() =>
      applyLowerImportsPlan_mutate(magicString, [
        {
          removeImport: { start: 0, end: 10 },
          references: [
            { kind: "namespace", alias: "ops", from: "@src/operations" },
          ],
        },
      ]),
    ).toThrow(SpecUserError);
  });
});
