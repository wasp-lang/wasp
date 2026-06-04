import { RolldownMagicString } from "rolldown";
import { parseAst } from "rolldown/parseAst";
import { describe, expect, test } from "vitest";
import { applyTransformImportsPlan_mutate } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/imports/apply.js";
import { planTransformImports } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/imports/plan.js";
import * as Fixtures from "../../spec/testFixtures.js";

describe("transformRefImports", () => {
  test("leaves files without ref imports untouched", () => {
    expect(transformImports([`const title = "Demo";`, ``].join("\n"))).toBe(
      [`const title = "Demo";`, ``].join("\n"),
    );
  });

  test.for([
    {
      caseName: "a default ref import",
      sourcePath: "/project/src/features/home.wasp.ts",
      sourceLines: [
        `import MainPage from "../MainPage" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { ref as ref } from "@wasp.sh/spec";`,
        `const MainPage = ref({"importDefault":"MainPage","from":"/project/src/MainPage"});`,
        ``,
        ``,
      ],
    },
    {
      caseName: "named ref imports",
      sourceLines: [
        `import { getTasks, createTask } from "./operations" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { ref as ref } from "@wasp.sh/spec";`,
        `const getTasks = ref({"import":"getTasks","alias":"getTasks","from":"/project/operations"});`,
        `const createTask = ref({"import":"createTask","alias":"createTask","from":"/project/operations"});`,
        ``,
        ``,
      ],
    },
    {
      caseName: "aliased named ref imports",
      sourceLines: [
        `import { archive as archiveTask } from "./operations" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { ref as ref } from "@wasp.sh/spec";`,
        `const archiveTask = ref({"import":"archive","alias":"archiveTask","from":"/project/operations"});`,
        ``,
        ``,
      ],
    },
    {
      caseName: "namespace ref imports",
      sourceLines: [
        `import * as adminOperations from "../adminOperations" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { ref as ref } from "@wasp.sh/spec";`,
        `const adminOperations = new Proxy({}, { get: (_t, k) => ref({ import: String(k), from: "/adminOperations", alias: "adminOperations_" + String(k) }) }) as Record<string, ReturnType<typeof ref>>;`,
        ``,
        ``,
      ],
    },
    {
      caseName: "a combined default and named ref import",
      sourceLines: [
        `import MainPage, { Helper } from "./src/MainPage" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { ref as ref } from "@wasp.sh/spec";`,
        `const MainPage = ref({"importDefault":"MainPage","from":"/project/src/MainPage"});`,
        `const Helper = ref({"import":"Helper","alias":"Helper","from":"/project/src/MainPage"});`,
        ``,
        ``,
      ],
    },
  ])("transforms $caseName", ({ sourceLines, expectedLines, sourcePath }) => {
    expect(transformImports(sourceLines.join("\n"), sourcePath)).toBe(
      expectedLines.join("\n"),
    );
  });

  test("transforms only ref imports in a mixed file", () => {
    expect(
      transformImports(
        [
          `import { app } from "@wasp.sh/spec";`,
          `import z from "zod";`,
          `import helper from "./helpers";`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          `export { helper } from "./helpers";`,
          `const title = "Demo";`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { ref as ref } from "@wasp.sh/spec";`,
        `const MainPage = ref({"importDefault":"MainPage","from":"/project/src/MainPage"});`,
        `import { app } from "@wasp.sh/spec";`,
        `import z from "zod";`,
        `import helper from "./helpers";`,
        ``,
        `export { helper } from "./helpers";`,
        `const title = "Demo";`,
        ``,
      ].join("\n"),
    );
  });

  test("generates a ref helper name that avoids top-level binding collisions", () => {
    expect(
      transformImports(
        [
          `const ref = "taken";`,
          `const ref1 = "taken";`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { ref as ref_0 } from "@wasp.sh/spec";`,
        `const MainPage = ref_0({"importDefault":"MainPage","from":"/project/src/MainPage"});`,
        `const ref = "taken";`,
        `const ref1 = "taken";`,
        ``,
        ``,
      ].join("\n"),
    );
  });

  test("avoids collisions with destructured and declared top-level bindings", () => {
    expect(
      transformImports(
        [
          `import { ref } from "./helpers";`,
          `const { ref1, nested: { ref2 }, ...ref3 } = helpers;`,
          `function ref4() {}`,
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { ref as ref_0 } from "@wasp.sh/spec";`,
        `const MainPage = ref_0({"importDefault":"MainPage","from":"/project/src/MainPage"});`,
        `import { ref } from "./helpers";`,
        `const { ref1, nested: { ref2 }, ...ref3 } = helpers;`,
        `function ref4() {}`,
        ``,
        ``,
      ].join("\n"),
    );
  });
});

function transformImports(
  sourceText: string,
  sourcePath = Fixtures.MOCK_MAIN_WASP_TS_PATH,
): string {
  const ast = parseAst(sourceText, { lang: "ts" });
  const source = new RolldownMagicString(sourceText);

  applyTransformImportsPlan_mutate(
    source,
    planTransformImports(ast, { importingFilePath: sourcePath }),
  );

  return source.toString();
}
