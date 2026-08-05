import { RolldownMagicString } from "rolldown";
import { parseAst } from "rolldown/parseAst";
import { describe, expect, test } from "vitest";
import { transformRefHelper_mutate } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/refHelper/index.js";
import { WaspSpecUserError } from "../../../src/spec/waspSpecUserError.js";

describe("transformRefHelper", () => {
  test("leaves files without a public ref import untouched", () => {
    expect(transformRefHelper([`const title = "Demo";`, ``].join("\n"))).toBe(
      [`const title = "Demo";`, ``].join("\n"),
    );
  });

  test("rewrites a public ref import into the internal helper", () => {
    expect(
      transformRefHelper(
        [
          `import { ref } from "@wasp.sh/spec";`,
          `const MainPage = ref({ importDefault: "MainPage", from: "./src/MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `const ref = _waspMakeRef("/path/main.wasp.ts");`,
        ``,
        `const MainPage = ref({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );
  });

  test("preserves the alias of an aliased public ref import", () => {
    expect(
      transformRefHelper(
        [
          `import { ref as appRef } from "@wasp.sh/spec";`,
          `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `const appRef = _waspMakeRef("/path/main.wasp.ts");`,
        ``,
        `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );
  });

  test("generates helper names that avoid top-level binding collisions", () => {
    expect(
      transformRefHelper(
        [
          `const ref = "taken";`,
          `const _waspMakeRef = "taken";`,
          `import { ref as appRef } from "@wasp.sh/spec";`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef as _waspMakeRef_0 } from "@wasp.sh/spec/internal";`,
        `const appRef = _waspMakeRef_0("/path/main.wasp.ts");`,
        `const ref = "taken";`,
        `const _waspMakeRef = "taken";`,
        ``,
        ``,
      ].join("\n"),
    );
  });

  test("treats a class declaration as a top-level binding collision", () => {
    expect(
      transformRefHelper(
        [
          `class _waspMakeRef {}`,
          `import { ref as appRef } from "@wasp.sh/spec";`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef as _waspMakeRef_0 } from "@wasp.sh/spec/internal";`,
        `const appRef = _waspMakeRef_0("/path/main.wasp.ts");`,
        `class _waspMakeRef {}`,
        ``,
        ``,
      ].join("\n"),
    );
  });

  test("handles multiple ref imports in the same file", () => {
    expect(
      transformRefHelper(
        [
          `import { ref as refA, ref as refB } from "@wasp.sh/spec";`,
          `import { ref as refC } from "@wasp.sh/spec";`,
          `import { ref } from "@wasp.sh/spec";`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `const refA = _waspMakeRef("/path/main.wasp.ts");`,
        `const refB = refA;`,
        `const refC = refA;`,
        `const ref = refA;`,
        ``,
        ``,
        ``,
        ``,
        // This is not a mistake: each newline is a removed import statement.
        // The ranges in the AST don't cover the final newline of each.
      ].join("\n"),
    );
  });

  test("preserves remaining @wasp.sh/spec specifiers when rewriting the ref import", () => {
    expect(
      transformRefHelper(
        [
          `import { type RefObject, ref as appRef, page } from "@wasp.sh/spec";`,
          `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `const appRef = _waspMakeRef("/path/main.wasp.ts");`,
        `import { type RefObject, page } from "@wasp.sh/spec";`,
        `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );
  });

  test("leaves re-exports from other modules untouched", () => {
    const source = `export { ref } from "./helpers";\n`;
    expect(transformRefHelper(source)).toBe(source);
  });

  test("rewrites a ref import that is then re-exported", () => {
    expect(
      transformRefHelper(
        [`import { ref } from "@wasp.sh/spec";`, `export { ref };`, ``].join(
          "\n",
        ),
      ),
    ).toBe(
      [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `const ref = _waspMakeRef("/path/main.wasp.ts");`,
        ``,
        `export { ref };`,
        ``,
      ].join("\n"),
    );
  });

  test("throws when transforming re-exports of the ref helper", () => {
    expect(() =>
      transformRefHelper(`export { ref } from "@wasp.sh/spec";`),
    ).toThrow(WaspSpecUserError);
  });
});

function transformRefHelper(sourceText: string): string {
  const ast = parseAst(sourceText, { lang: "ts" });
  const source = new RolldownMagicString(sourceText);

  transformRefHelper_mutate("/path/main.wasp.ts", ast, source);

  return source.toString();
}
