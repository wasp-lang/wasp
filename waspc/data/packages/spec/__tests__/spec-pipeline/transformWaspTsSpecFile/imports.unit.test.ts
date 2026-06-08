import { RolldownMagicString } from "rolldown";
import { parseAst } from "rolldown/parseAst";
import { describe, expect, test } from "vitest";
import { transformRefImports_mutate } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/imports/index.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";

describe("transformRefImports", () => {
  test("leaves files without ref imports untouched", () => {
    expect(transformImports([`const title = "Demo";`, ``].join("\n"))).toBe(
      [`const title = "Demo";`, ``].join("\n"),
    );
  });

  test.for([
    {
      caseName: "a default ref import",
      sourceLines: [
        `import MainPage from "../MainPage" with { type: "ref" };`,
        ``,
      ],
      expectedLines: [
        `import { ref } from "@wasp.sh/spec";`,
        `const MainPage = ref({"importDefault":"MainPage","from":"../MainPage"});`,
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
        `import { ref } from "@wasp.sh/spec";`,
        `const getTasks = ref({"import":"getTasks","alias":"getTasks","from":"./operations"});`,
        `const createTask = ref({"import":"createTask","alias":"createTask","from":"./operations"});`,
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
        `import { ref } from "@wasp.sh/spec";`,
        `const archiveTask = ref({"import":"archive","alias":"archiveTask","from":"./operations"});`,
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
        `import { ref } from "@wasp.sh/spec";`,
        `const MainPage = ref({"importDefault":"MainPage","from":"./src/MainPage"});`,
        `const Helper = ref({"import":"Helper","alias":"Helper","from":"./src/MainPage"});`,
        ``,
        ``,
      ],
    },
  ])("transforms $caseName", ({ sourceLines, expectedLines }) => {
    expect(transformImports(sourceLines.join("\n"))).toBe(
      expectedLines.join("\n"),
    );
  });

  test("rejects namespace ref imports with a SpecUserError", () => {
    expect(() =>
      transformImports(
        [
          `import * as adminOperations from "../adminOperations" with { type: "ref" };`,
          ``,
        ].join("\n"),
      ),
    ).toThrow(SpecUserError);
  });

  test("mentions the offending namespace binding in the error", () => {
    expect(() =>
      transformImports(
        [
          `import * as adminOperations from "../adminOperations" with { type: "ref" };`,
          ``,
        ].join("\n"),
      ),
    ).toThrow(/import \* as adminOperations/);
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
        `import { ref } from "@wasp.sh/spec";`,
        `const MainPage = ref({"importDefault":"MainPage","from":"./src/MainPage"});`,
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
        `const MainPage = ref_0({"importDefault":"MainPage","from":"./src/MainPage"});`,
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
        `const MainPage = ref_0({"importDefault":"MainPage","from":"./src/MainPage"});`,
        `import { ref } from "./helpers";`,
        `const { ref1, nested: { ref2 }, ...ref3 } = helpers;`,
        `function ref4() {}`,
        ``,
        ``,
      ].join("\n"),
    );
  });

  describe("ref imports without specifiers", () => {
    test.for([
      {
        caseName: "a side-effect ref import",
        source: `import "./operations" with { type: "ref" };`,
      },
      {
        caseName: "an empty named ref import",
        source: `import {} from "./operations" with { type: "ref" };`,
      },
    ])("rejects $caseName", ({ source }) => {
      expect(() => transformImports(source)).toThrow(SpecUserError);
      expect(() => transformImports(source)).toThrow(
        "must import at least one binding",
      );
    });
  });

  test("leaves re-exports without the ref attribute untouched", () => {
    const source = `export { helper } from "./helpers";\n`;
    expect(transformImports(source)).toBe(source);
  });

  test("transforms a ref import that is then re-exported", () => {
    expect(
      transformImports(
        [
          `import MainPage from "./src/MainPage" with { type: "ref" };`,
          `export { MainPage };`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { ref } from "@wasp.sh/spec";`,
        `const MainPage = ref({"importDefault":"MainPage","from":"./src/MainPage"});`,
        ``,
        `export { MainPage };`,
        ``,
      ].join("\n"),
    );
  });

  test("throws when transforming re-exports with ref imports", () => {
    expect(() =>
      transformImports(
        `export { MainPage } from "./src/MainPage" with { type: "ref" };`,
      ),
    ).toThrow(SpecUserError);
  });
});

function transformImports(sourceText: string): string {
  const ast = parseAst(sourceText, { lang: "ts" });
  const source = new RolldownMagicString(sourceText);

  transformRefImports_mutate(ast, source);

  return source.toString();
}
