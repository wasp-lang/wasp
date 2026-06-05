import { RolldownMagicString } from "rolldown";
import { parseAst } from "rolldown/parseAst";
import { describe, expect, test } from "vitest";
import { applyTransformRefHelperPlan_mutate } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/refHelper/apply.js";
import { planTransformRefHelper } from "../../../src/spec-pipeline/transformWaspTsSpecFilesPlugin/refHelper/plan.js";

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
        `const ref_0 = _waspMakeRef(import.meta.url);`,
        `const ref = ref_0;`,
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
        `const ref = _waspMakeRef(import.meta.url);`,
        `const appRef = ref;`,
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
        `const ref_0 = _waspMakeRef_0(import.meta.url);`,
        `const appRef = ref_0;`,
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
        `const ref = _waspMakeRef_0(import.meta.url);`,
        `const appRef = ref;`,
        `class _waspMakeRef {}`,
        ``,
        ``,
      ].join("\n"),
    );
  });

  test("treats type-only imports that shadow the helper names as collisions", () => {
    expect(
      transformRefHelper(
        [
          `import type { ref, _waspMakeRef } from "./types";`,
          `import { ref as appRef } from "@wasp.sh/spec";`,
          ``,
        ].join("\n"),
      ),
    ).toBe(
      [
        `import { _waspMakeRef as _waspMakeRef_0 } from "@wasp.sh/spec/internal";`,
        `const ref_0 = _waspMakeRef_0(import.meta.url);`,
        `const appRef = ref_0;`,
        `import type { ref, _waspMakeRef } from "./types";`,
        ``,
        ``,
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
        `const ref = _waspMakeRef(import.meta.url);`,
        `const appRef = ref;`,
        `import { type RefObject, page } from "@wasp.sh/spec";`,
        `const MainPage = appRef({ importDefault: "MainPage", from: "./src/MainPage" });`,
        ``,
      ].join("\n"),
    );
  });
});

function transformRefHelper(sourceText: string): string {
  const ast = parseAst(sourceText, { lang: "ts" });
  const source = new RolldownMagicString(sourceText);

  const plan = planTransformRefHelper(ast);
  if (plan) {
    applyTransformRefHelperPlan_mutate(source, plan);
  }

  return source.toString();
}
