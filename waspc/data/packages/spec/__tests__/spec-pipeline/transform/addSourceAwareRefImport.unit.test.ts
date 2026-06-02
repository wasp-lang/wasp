import { describe, expect, test } from "vitest";
import { addSourceAwareRefImport } from "../../../src/spec-pipeline/transform/addSourceAwareRefImport.js";
import { SpecUserError } from "../../../src/spec/specUserError.js";
import * as Fixtures from "../../spec/testFixtures.js";

describe("addSourceAwareRefImport", () => {
  test("rewrites explicit ref imports", () => {
    expect(
      transform(
        [
          `import { ref, page } from "@wasp.sh/spec";`,
          `const MainPage = ref({ importDefault: "MainPage", from: "./MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refName: "ref",
      sourceText: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef(import.meta.url);`,
        `import { page } from "@wasp.sh/spec";`,
        `const MainPage = ref({ importDefault: "MainPage", from: "./MainPage" });`,
        ``,
      ].join("\n"),
    });
  });

  test("throws when there is no spec package import", () => {
    const localSpecPackageImport =
      "file:///repo/waspc/data/packages/spec/src/spec/publicApi/index.ts";
    const sourceText = [
      `import { ref, page } from ${JSON.stringify(localSpecPackageImport)};`,
      `const MainPage = ref({ importDefault: "MainPage", from: "./MainPage" });`,
      ``,
    ].join("\n");

    expect(() => transform(sourceText)).toThrowError(SpecUserError);
  });

  test("preserves a ref alias", () => {
    expect(
      transform(
        [
          `import { ref as appRef, page } from "@wasp.sh/spec";`,
          `const MainPage = appRef({ importDefault: "MainPage", from: "./MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refName: "appRef",
      sourceText: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const appRef = _waspMakeRef(import.meta.url);`,
        `import { page } from "@wasp.sh/spec";`,
        `const MainPage = appRef({ importDefault: "MainPage", from: "./MainPage" });`,
        ``,
      ].join("\n"),
    });
  });

  test("uses generated names that do not collide with top-level bindings", () => {
    expect(
      transform(
        [
          `import { app } from "@wasp.sh/spec";`,
          `const ref = "taken";`,
          `const ref1 = "taken";`,
          `const _waspMakeRef = "taken";`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refName: "ref2",
      sourceText: [
        `import { _waspMakeRef as _waspMakeRef1 } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref2 = _waspMakeRef1(import.meta.url);`,
        `import { app } from "@wasp.sh/spec";`,
        `const ref = "taken";`,
        `const ref1 = "taken";`,
        `const _waspMakeRef = "taken";`,
        ``,
      ].join("\n"),
    });
  });

  test("aliases the factory import when a ref alias uses the factory name", () => {
    expect(
      transform(
        [
          `import { ref as _waspMakeRef, page } from "@wasp.sh/spec";`,
          `const MainPage = _waspMakeRef({ importDefault: "MainPage", from: "./MainPage" });`,
          ``,
        ].join("\n"),
      ),
    ).toEqual({
      refName: "_waspMakeRef",
      sourceText: [
        `import { _waspMakeRef as _waspMakeRef1 } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const _waspMakeRef = _waspMakeRef1(import.meta.url);`,
        `import { page } from "@wasp.sh/spec";`,
        `const MainPage = _waspMakeRef({ importDefault: "MainPage", from: "./MainPage" });`,
        ``,
      ].join("\n"),
    });
  });

  test("preserves type specifiers in mixed imports", () => {
    expect(
      transform(
        [`import { type RefObject, ref, page } from "@wasp.sh/spec";`, ``].join(
          "\n",
        ),
      ),
    ).toEqual({
      refName: "ref",
      sourceText: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef(import.meta.url);`,
        `import { type RefObject, page } from "@wasp.sh/spec";`,
        ``,
      ].join("\n"),
    });
  });

  test("adds the helper when there is only a type-only spec package import", () => {
    const sourceText = `import type { RefObject } from "@wasp.sh/spec";\n`;

    expect(transform(sourceText)).toEqual({
      refName: "ref",
      sourceText: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef(import.meta.url);`,
        `import type { RefObject } from "@wasp.sh/spec";`,
        ``,
      ].join("\n"),
    });
  });

  test.each([
    [`import { _waspMakeRef, ref, page } from "@wasp.sh/spec";\n`],
    [`import { _waspMakeRef as makeRef, ref, page } from "@wasp.sh/spec";\n`],
    [`import { type _waspMakeRef, ref, page } from "@wasp.sh/spec";\n`],
    [`import type { _waspMakeRef } from "@wasp.sh/spec";\n`],
  ])("rejects internal helper import: %s", (sourceText) => {
    expect(() => transform(sourceText)).toThrowError(SpecUserError);
  });

  test("adds ref helper for spec package value imports", () => {
    expect(transform(`import { app } from "@wasp.sh/spec";\n`)).toEqual({
      refName: "ref",
      sourceText: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef(import.meta.url);`,
        `import { app } from "@wasp.sh/spec";`,
        ``,
      ].join("\n"),
    });
  });

  test("allows namespace spec package imports", () => {
    expect(transform(`import * as wasp from "@wasp.sh/spec";\n`)).toEqual({
      refName: "ref",
      sourceText: [
        `import { _waspMakeRef } from "@wasp.sh/spec/internal";`,
        `// @ts-ignore TS6133: This generated helper can be unused in files without refs.`,
        `const ref = _waspMakeRef(import.meta.url);`,
        `import * as wasp from "@wasp.sh/spec";`,
        ``,
      ].join("\n"),
    });
  });

  function transform(sourceText: string) {
    return addSourceAwareRefImport({
      sourceText,
      sourcePath: Fixtures.MOCK_MAIN_WASP_TS_PATH,
    });
  }
});
