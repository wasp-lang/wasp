import { describe, expect, test } from "vitest";
import {
  planLowerImports,
  PlannedImport,
} from "../../../src/spec-pipeline/lowerImportsPlugin/plan.js";

describe("planLowerImports", () => {
  describe("simple cases", () => {
    type SimpleTestCase = [
      name: string,
      source: string[],
      expected: PlannedImport[],
    ];

    test.for([
      [
        'plans a default ref import as an `import: "default"` binding',
        [`import MainPage from "./src/MainPage" with { type: "ref" };`],
        [
          {
            removeImport: { start: 0, end: 59 },
            references: [
              {
                kind: "default",
                refObject: { importDefault: "MainPage", from: "@src/MainPage" },
              },
            ],
          },
        ],
      ],
      [
        "plans named ref imports preserving the imported name as the alias",
        [
          `import { getTasks, createTask } from "./src/operations" with { type: "ref" };`,
        ],
        [
          {
            removeImport: { start: 0, end: 77 },
            references: [
              {
                kind: "named",
                refObject: {
                  import: "getTasks",
                  alias: "getTasks",
                  from: "@src/operations",
                },
              },
              {
                kind: "named",
                refObject: {
                  import: "createTask",
                  alias: "createTask",
                  from: "@src/operations",
                },
              },
            ],
          },
        ],
      ],
      [
        "plans aliased named ref imports with the local name as the alias",
        [
          `import { archive as archiveTask } from "./src/operations" with { type: "ref" };`,
        ],
        [
          {
            removeImport: { start: 0, end: 79 },
            references: [
              {
                kind: "named",
                refObject: {
                  import: "archive",
                  alias: "archiveTask",
                  from: "@src/operations",
                },
              },
            ],
          },
        ],
      ],
      [
        "plans namespace ref imports",
        [`import * as ops from "./src/operations" with { type: "ref" };`],
        [
          {
            removeImport: { start: 0, end: 61 },
            references: [
              { kind: "namespace", alias: "ops", from: "@src/operations" },
            ],
          },
        ],
      ],
      [
        "plans a combined default + named ref import in a single statement",
        [
          `import MainPage, { Helper } from "./src/MainPage" with { type: "ref" };`,
        ],
        [
          {
            removeImport: { start: 0, end: 71 },
            references: [
              {
                kind: "default",
                refObject: { importDefault: "MainPage", from: "@src/MainPage" },
              },
              {
                kind: "named",
                refObject: {
                  import: "Helper",
                  alias: "Helper",
                  from: "@src/MainPage",
                },
              },
            ],
          },
        ],
      ],
      [
        "plans nothing for non-ref imports and re-exports",
        [
          `import { app } from "@wasp.sh/spec";`,
          `import z from "zod";`,
          `import helper from "./helpers";`,
          `import MainPage from "@src/MainPage";`,
          `export { helper } from "./helpers";`,
        ],
        [],
      ],
      [
        "plans only the ref imports in a mixed file",
        [
          `import { app } from "@wasp.sh/spec";`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          `import { getTasks } from "./src/operations" with { type: "ref" };`,
          `import helper from "./helpers";`,
        ],
        [
          {
            removeImport: { start: 37, end: 96 },
            references: [
              {
                kind: "default",
                refObject: { importDefault: "MainPage", from: "@src/MainPage" },
              },
            ],
          },
          {
            removeImport: { start: 97, end: 162 },
            references: [
              {
                kind: "named",
                refObject: {
                  import: "getTasks",
                  alias: "getTasks",
                  from: "@src/operations",
                },
              },
            ],
          },
        ],
      ],
      [
        "records the source span of each ref import for replacement",
        [
          `import { app } from "@wasp.sh/spec";`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
        ],
        [
          {
            removeImport: { start: 37, end: 96 },
            references: [
              {
                kind: "default",
                refObject: { importDefault: "MainPage", from: "@src/MainPage" },
              },
            ],
          },
        ],
      ],
    ] satisfies SimpleTestCase[])(
      "%s",
      ([, source, expected]: SimpleTestCase) => {
        expect(
          planLowerImports({
            sourceText: source.join("\n"),
            projectRootDir: "/project",
            importingFilePath: "/project/main.wasp.ts",
          }),
        ).toEqual(expected);
      },
    );
  });

  test("resolves ref imports relative to the importing file's directory", () => {
    const sourceText = `import MainPage from "./MainPage" with { type: "ref" };`;

    const plan = planLowerImports({
      sourceText,
      projectRootDir: "/project",
      importingFilePath: "/project/src/features/home.wasp.ts",
    });

    expect(plan).toEqual([
      {
        removeImport: { start: 0, end: 55 },
        references: [
          {
            kind: "default",
            refObject: {
              importDefault: "MainPage",
              from: "@src/features/MainPage",
            },
          },
        ],
      },
    ]);
  });
});
